;;;; clgl.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(deftype point ()
  '(or vec3))

(deftype normal ()
  '(or vec3))

(deftype color ()
  '(or vec4))

(defun red (color)
  (vx color))

(defun green (color)
  (vy color))

(defun blue (color)
  (vz color))

(defun alpha (color)
  (vw color))

(defun read-file (fname)
  (with-open-file (ins fname)
    (let ((seq (make-array (file-length ins)
                           :element-type (stream-element-type ins))))
      (read-sequence seq ins) seq)))

(defgeneric get-transform-matrix (view))

(defclass spherical-viewport ()
  ((radius :initarg :radius :initform 10.0f0)
   (theta :initarg :theta :initform 0.0f0)
   (phi :initarg :phi :initform (/ pi 4))))

(defmethod get-transform-matrix ((view spherical-viewport))
  (marr4 (meye 4)))

(defclass look-at-viewport ()
  ((eye :initarg :eye :initform (vec3 0.0f0 0.0f0 10.0f0))
   (center :initarg :center :initform (vec3 0.0f0 0.0f0 0.0f0))
   (up :initarg :up :initform +vy+)))

(defmethod get-transform-matrix ((view look-at-viewport))
  (with-slots (eye center up) view
    (let ((result (marr4 (minv (nmlookat (m* 0.125 (meye 4)) eye center up)))))
      ;;(format t "~a~%" result)
      result)))

(defclass scene ()
  ((viewport :initarg :viewport :initform (make-instance 'look-at-viewport))
   (objects :initarg :objects :initform (list (make-instance 'primitive-container)))
   (modified :initform t)
   (lock :initform (bt:make-lock "scene-lock")))
  (:documentation ""))

(defmacro with-scene-lock ((scene) &body body)
  `(bt:with-lock-held ((slot-value ,scene 'clgl::lock))
     ,@body))

(defmacro with-primitives ((scene prims) &body body)
  `(with-scene-lock (,scene)
     (let ((,prims (car (slot-value ,scene 'objects))))
       ,@body)
     (setf (slot-value ,scene 'clgl::modified) t)))

(defun force-redraw (scene)
  (with-scene-lock (scene)
    (setf (slot-value scene 'clgl::modified) t)))
  
(defun render (scene)
  (with-slots (objects viewport modified lock) scene
    (dolist (object objects)
      (when modified
        (fill-buffers object)
        (setf modified nil))
      (gl:enable :line-smooth :polygon-smooth
                 :depth-test :depth-clamp)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer :depth-buffer)
      (render-buffers object viewport))))

(defgeneric cleanup (object))

(defmethod cleanup ((object scene))
  (format t "Cleaning up scene~%")
  (with-slots (objects) object
    (dolist (obj objects)
      (cleanup obj))))

(defun create-scene ()
  (make-instance 'scene))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun scene-viewer (scene)
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (coerce (getf cur-mode '%cl-glfw3:width) 'single-float))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (with-window (:title "OpenGL Scene Viewer"
                           :width (floor (/ cur-width 2.0f0))
                           :height cur-height
                           :decorated nil
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :resizable nil)

        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (gl:clear-color 0 0 0 1.0)

        ;; The 'event loop'
        (loop until (window-should-close-p)
           do
             (bt:with-lock-held ((slot-value scene 'lock))
               (render scene))
           do (swap-buffers)
           do (poll-events))
        ;; Finally clean up
        (cleanup scene)))))

(defun view-scene (scene &optional (in-main-thread t))
  (if in-main-thread
      (trivial-main-thread:with-body-in-main-thread ()
        (scene-viewer scene))
      (scene-viewer scene)))

(defun create-and-view (&optional (background nil))
  (let ((scene (clgl:create-scene)))
    (with-primitives (scene prims)
      ;; (dotimes (i 200)
      ;;   (clgl:add-point prims
      ;;                   (vec3-random -0.5f0 0.5f0)
      ;;                   (vec 0.0f0 0.0f0 1.0f0 1.0f0)))
      (dotimes (i 10)
        (clgl:add-line prims
                       (vec3-random -0.5f0 0.5f0)
                       (vec3-random -0.5f0 0.5f0)
                       (vec 0.0f0 1.0f0 0.0f0 1.0f0))
        )
      ;; (dotimes (i 2)
      ;;   (clgl:add-triangle prims
      ;;                      (vec3-random -0.5f0 0.5f0)
      ;;                      (vec3-random -0.5f0 0.5f0)
      ;;                      (vec3-random -0.5f0 0.5f0)
      ;;                      (vec 1.0f0 0.0f0 0.0f0 1.0f0)
      ;;                      :filled nil))
      ;; (dotimes (i 2)
      ;;   (clgl:add-triangle prims
      ;;                      (vec3-random -0.5f0 0.5f0)
      ;;                      (vec3-random -0.5f0 0.5f0)
      ;;                      (vec3-random -0.5f0 0.5f0)
      ;;                      (vec 0.7f0 0.0f0 0.8f0 1.0f0)
      ;;                      :filled t))
      )
    (view-scene scene background)
    scene))
