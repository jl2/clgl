;;;; shaders.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defparameter *shader-dir* "/home/jeremiah/src/lisp/clgl/shaders/")

(defclass shader-program ()
  ((vertex-text :initarg :vertex :initform (read-file (merge-pathnames *shader-dir* "default-vertex.glsl")))
   (fragment-text :initarg :fragment :initform (read-file (merge-pathnames *shader-dir* "default-fragment.glsl")))
   (vertex-shader :initform 0)
   (fragment-shader :initform 0)
   (program :initform nil)))

(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (vert-shader frag-shader program) obj
    (when (> 0 vert-shader)
      (gl:delete-shader vert-shader))
    (when (> 0 frag-shader)
      (gl:delete-shader vert-shader))
    (when (> 0 vert-shader)
      (gl:delete-shader vert-shader))))

(defun compile-shader (type text)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader text)
    (gl:compile-shader shader)
    (when (not (eq t (gl:get-shader shader :compile-status)))
      (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
      (format t "info-log ~a~%" (gl:get-shader-info-log shader)))
    shader))

(defun build-program (program)
  (with-slots (vertex-text fragment-text vertex-shader fragment-shader program) program
    (setf vertex-shader (compile-shader :vertex-shader vertex-text))
    (setf fragment-shader (compile-shader :fragment-shader fragment-text))
    (setf program (gl:create-program))
      (gl:attach-shader program vertex-shader)
      (gl:attach-shader program fragment-shader)
      (gl:link-program program)

      (let ((status (gl:get-program program :link-status)))
        (format t "link-program: ~a~%~a~%" status(gl:get-program-info-log program)))

      (gl:validate-program program)
      (let ((status (gl:get-program program :validate-status)))
        (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log program)))))

(defun use-program (program)
  (with-slots (program) program
    (let* ((float-size   (cffi:foreign-type-size :float))
           (stride       (* (+ 3 3 4) float-size))
           (position-offset  (* 0 float-size))
           (normal-offset  (* 3 float-size))
           (color-offset (* 6 float-size))
           (position-attrib (gl:get-attrib-location program "position"))
           (normal-attrib (gl:get-attrib-location program "normal"))
           (color-attrib (gl:get-attrib-location program "color")))
      
      (gl:enable-vertex-attrib-array position-attrib)
      (gl:enable-vertex-attrib-array normal-attrib)
      (gl:enable-vertex-attrib-array color-attrib)
      (gl:vertex-attrib-pointer position-attrib 3 :float :false stride position-offset)
      (gl:vertex-attrib-pointer normal-attrib 3 :float :false stride normal-offset)
      (gl:vertex-attrib-pointer color-attrib 4 :float :false stride color-offset)

      (gl:use-program program))))
