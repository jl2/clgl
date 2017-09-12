
(in-package #:clgl)

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
  (with-slots (shader-program) object
    (when (null (slot-value shader-program 'program))
      (build-program shader-program))))

(defmethod render-buffers ((object opengl-object) viewport)
  (with-slots (vao shader-program) object
    (when (> 0 vao)
      (gl:bind-vertex-array vao)
      (with-slots (program) shader-program
        (gl:uniform-matrix (gl:get-uniform-location program "projectionMatrix")
	                       4 
                           (vector (3d-matrices:marr4 (get-transform-matrix viewport))))
        (use-program shader-program)))))

(defmethod cleanup ((object opengl-object))
  (declare (ignorable object))
  (format t "Cleaning up opengl-object: ~a~%" object)
  (with-slots (vao vbos data shaders shader-program) object
    (when (/= 0 vao)
      (when vbos
        (gl:delete-buffers vbos))
      (when (> 0 shader-program)
        (gl:delete-program shader-program))
      (dolist (shader shaders)
        (cleanup shader))
      (gl:delete-vertex-arrays (list vao)))))


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
