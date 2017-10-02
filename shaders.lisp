;;;; shaders.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defparameter *shader-dir* "~/src/lisp/clgl/shaders/")

(defclass shader-program ()
  ((layout :initarg :inputs)
   (vertex-text :initarg :vertex)
   (fragment-text :initarg :fragment)
   (vertex-shader :initform 0)
   (fragment-shader :initform 0)
   (program :initform 0)))

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

(defun compile-shader (shader text)
  (gl:shader-source shader text)
  (gl:compile-shader shader)
  (when (not (eq t (gl:get-shader shader :compile-status)))
    (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
    (format t "info-log ~a~%" (gl:get-shader-info-log shader))))

(defun build-program (program)
  (with-slots (vertex-text fragment-text vertex-shader fragment-shader program) program

    (if (zerop vertex-shader)
        (setf vertex-shader (gl:create-shader :vertex-shader))
        (when (not (zerop program))
          (gl:detach-shader program vertex-shader)))
    (compile-shader vertex-shader vertex-text)

    (if (zerop fragment-shader)
        (setf fragment-shader (gl:create-shader :fragment-shader))
        (when (not (zerop program))
          (gl:detach-shader program fragment-shader)))
    (compile-shader fragment-shader fragment-text)

    (when (zerop program)
      (setf program (gl:create-program)))

    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)

    (let ((status (gl:get-program program :link-status)))
      (when (not (eq status t))
        (format t "link-program: ~a~%~a~%" status (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq status t))
        (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log program))))))


(defun use-program (shader-program transformation viewport)
  (with-slots (layout program) shader-program
    (let* ((float-size   (cffi:foreign-type-size :float))
           (stride       (* (apply #'+ (mapcar #'cdr layout)) float-size)))
      (loop for offset = 0 then (incf offset (cdr entry))
         for entry in layout
         for count from 0
         do
           (let* ((attrib-name (car entry))
                  (attrib-size (cdr entry))
                  (position-offset offset)
                  (position-attrib (gl:get-attrib-location program attrib-name)))
             (when (>= position-attrib 0)
               (gl:enable-vertex-attrib-array position-attrib)
               (gl:vertex-attrib-pointer position-attrib attrib-size :float :false stride (* float-size position-offset))))))

    (gl:use-program program)
    (let ((xform-location (gl:get-uniform-location program "transformationMatrix")))
      (gl:uniform-matrix xform-location
                         4
                         (vector (marr4 (m*
                                         (get-transform-matrix viewport)
                                         transformation)))
                         t))))

