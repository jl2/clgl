;;;; shaders.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defparameter *shader-dir* "~/src/lisp/clgl/shaders/")

(defclass shader-program ()
  ((layout :initarg :inputs :initform '( ("position" . 3) ("color" . 4)))
   (vertex-text :initarg :vertex :initform (read-file (merge-pathnames *shader-dir* "default-line-vertex.glsl")))
   (fragment-text :initarg :fragment :initform (read-file (merge-pathnames *shader-dir* "default-fragment.glsl")))
   (vertex-shader :initform 0)
   (fragment-shader :initform 0)
   (program :initform nil)))

(defmethod cleanup ((obj shader-program))
  "Delete a shader on the GPU."
  (with-slots (vertex-shader fragment-shader program) obj
    (when (> 0 vertex-shader)
      (gl:delete-shader vertex-shader))
    (when (> 0 fragment-shader)
      (gl:delete-shader vertex-shader))
    (when (> 0 program)
      (gl:delete-program program))

    (setf vertex-shader 0)
    (setf fragment-shader 0)
    (setf program 0)))

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


(defun use-program (shader-program transformation viewport)
  (with-slots (layout program) shader-program
    (let* ((float-size   (cffi:foreign-type-size :float))
           (stride       (* (apply #'+ (mapcar #'cdr layout)) float-size)))
      (loop for entry in layout
         for count from 0
         do
           (let* ((attrib-name (car entry))
                  (attrib-size (cdr entry))
                  (position-offset  (* count float-size))
                  (position-attrib (gl:get-attrib-location program attrib-name)))
             (gl:enable-vertex-attrib-array position-attrib)
             (gl:vertex-attrib-pointer position-attrib attrib-size :float :false stride position-offset))))
    (gl:use-program program)
    (let ((xform-location (gl:get-uniform-location program "transformationMatrix"))
          (proj-location (gl:get-uniform-location program "projectionMatrix")))
      (gl:uniform-matrix xform-location 4 (vector
                                           (marr4 (m*

                                                   transformation
                                                   (get-transform-matrix viewport)
                                                   )))
                         t)
      (gl:uniform-matrix proj-location 4 (vector
                                          (marr4 (get-projection-matrix viewport)))
                         t))))

