;;;; stl-file.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defclass stl-file (opengl-object)
  ((file-name :initarg :file-name)
   (draw-style :initarg :style :initform :line)
   (triangle-count :initform 0))
  (:documentation "An STL file."))


(defmethod fill-buffers ((object stl-file))
  (call-next-method)
  (with-slots (vao vbos ebos file-name triangle-count) object
    (when (null vbos)
      (setf vbos (gl:gen-buffers 1))
      (setf ebos (gl:gen-buffers 1)))
    (multiple-value-bind (v-data idxs) (stl:to-opengl (stl:read-stl file-name))
      (let ((gl-vertices (to-gl-float-array v-data)))
        (gl:bind-buffer :array-buffer (car vbos))
        (gl:buffer-data :array-buffer :dynamic-draw gl-vertices)
        (gl:free-gl-array gl-vertices))
      (let ((gl-indices (to-gl-array idxs :unsigned-int)))
        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:buffer-data :element-array-buffer :dynamic-draw gl-indices)
        (gl:free-gl-array gl-indices)
        (setf triangle-count (length idxs))))))

(defmethod render ((object stl-file) viewport frame)
  (declare (ignorable frame))
  (call-next-method)
  (with-slots (vbos ebos transformation draw-style triangle-count line-program fill-program) object

    (cond ((eq draw-style :point)
           (gl:polygon-mode :front-and-back :line)
           (gl:bind-buffer :array-buffer (car vbos))
           (use-program fill-program transformation viewport)
           (gl:bind-buffer :element-array-buffer (car ebos))
           (gl:draw-elements :points (gl:make-null-gl-array :unsigned-int) :count triangle-count))
          (t
           (gl:polygon-mode :front-and-back draw-style)
           (gl:bind-buffer :array-buffer (car vbos))
           (use-program fill-program transformation viewport)
           (gl:bind-buffer :element-array-buffer (car ebos))
           (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count triangle-count)))))
