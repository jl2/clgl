;;;; opengl-object.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)
(declaim (optimize (speed 0) (debug 3) (safety 3)))
(defclass opengl-object ()
  ((color :initarg :color :initform nil :type '(or null color))
   (visible :initarg :visible :initform t :type t)
   (material-id :initarg :regular :initform 0 :type fixnum)
   (vao :initform 0 :type fixnum)
   (vbos :initform nil :type cons)
   (ebos :initform nil :type cons)
   (shader-program :initform (make-instance 'shader-program
                                            :vertex (read-file (merge-pathnames *shader-dir* "default-vertex.glsl"))
                                            :fragment(read-file (merge-pathnames *shader-dir* "default-fragment.glsl")))))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric fill-buffers (object))
(defgeneric render-buffers (object viewport))

(defmethod fill-buffers ((object opengl-object))
  (with-slots (vao shader-program) object
    (when (= 0 vao)
      (setf vao (gl:gen-vertex-array))
      (format t "Allocated vao: ~a~%" vao))
    (format t "Binding vao ~a~%" vao)
    (gl:bind-vertex-array vao)
    (build-program shader-program)))

(defmethod fill-buffers :after ((object opengl-object))
  (with-slots (shader-program) object
    (use-program shader-program)
    (format t "Unbinding vertex array...~%")
    (gl:bind-vertex-array 0)))

(defmethod cleanup ((object opengl-object))
  (declare (ignorable object))
  (format t "Cleaning up opengl-object: ~a~%" object)
  (with-slots (vao vbos shader-program) object
    (when (/= 0 vao)
      (when vbos
        (gl:delete-buffers vbos))
      (cleanup shader-program)
      (gl:delete-vertex-arrays (list vao)))
    (setf vao 0)
    (setf vbos nil)))

(defmethod render-buffers ((object opengl-object) viewport)
  (with-slots (vao shader-program) object
    (when (/= 0 vao)
      (gl:bind-vertex-array vao)
      (let ((matrix (vector (get-transform-matrix viewport))))
        (with-slots (program) shader-program
          (gl:uniform-matrix
           (gl:get-uniform-location program "projectionMatrix")
	       4
           matrix
           ))))))




(defun to-gl-float-array (arr)
  "Create an OpenGL float array from a CL array of numbers.
   This is a convenience function that will coerce array elments to single-float."
  (declare (optimize (speed 3)))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array :float count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (coerce (aref arr i) 'single-float)))
    gl-array))

(defun to-gl-array (arr type)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3)))
  (let* ((count (length arr))
         (gl-array (gl:alloc-gl-array type count)))
    (dotimes (i count)
      (setf (gl:glaref gl-array i) (aref arr i)))
    gl-array))
