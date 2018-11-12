;;;; opengl-object.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (vbos :initform nil :type (or null cons))
   (ebos :initform nil :type (or null cons))
   (line-program :initform (make-instance
                      'shader-program
                      :inputs '(("position" . 3) ("color" . 4))
                      :vertex (read-file (merge-pathnames *shader-dir* "default-line-vertex.glsl"))
                      :fragment(read-file (merge-pathnames *shader-dir* "default-fragment.glsl")))
                 :initarg :line-program)
   (fill-program :initform
                   (make-instance
                      'shader-program
                      :inputs '(("position" . 3) ("normal" . 3) ("color" . 4))
                      :vertex (read-file (merge-pathnames *shader-dir* "default-filled-vertex.glsl"))
                      :fragment(read-file (merge-pathnames *shader-dir* "default-fragment.glsl")))
                   :initarg :fill-program)
   (transformation :initform (meye 4) :type mat4))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric fill-buffers (object))
(defgeneric render (object viewport frame))

(defgeneric rebuild-shaders (object))
(defmethod rebuild-shaders ((object opengl-object))
  (with-slots (vao line-program fill-program) object
    (when vao
      (gl:bind-vertex-array vao)
      (build-program line-program)
      (build-program fill-program))))

(defmethod fill-buffers ((object opengl-object))
  (with-slots (vao) object
    (when (= 0 vao)
      (setf vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)))

(defmethod fill-buffers :after ((object opengl-object))
  (gl:bind-vertex-array 0))

(defmethod cleanup ((object opengl-object))
  (declare (ignorable object))
  (format t "Cleanuing up ~a~%" object)
  (with-slots (vao vbos ebos line-program fill-program) object
    (when (/= 0 vao)
      (when vbos
        (gl:delete-buffers vbos))
      (when ebos
        (gl:delete-buffers ebos))
      (cleanup line-program)
      (cleanup fill-program)
      (gl:delete-vertex-arrays (list vao)))
    (setf vao 0)
    (setf vbos nil)))

(defmethod render ((object opengl-object) viewport frame)
  (declare (ignorable frame))
  (with-slots (vao transformation) object
    (when (/= 0 vao)
      (gl:bind-vertex-array vao))))

(defmethod render :after ((object opengl-object) viewport frame)
  (declare (ignorable frame))
  (gl:bind-vertex-array 0))

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
