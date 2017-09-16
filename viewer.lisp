;;;; viewer.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass viewer ()
  ((objects :initform nil :type (or null cons))
   (viewport :initarg :viewport :initform (make-instance 'look-at-viewport))
   (ticker-callback :initform nil)
   (modified :initform t)
   (window-open :initform nil)
   (should-close :initform nil)
   (to-cleanup :initform nil)
   (viewer-lock :initform (bt:make-lock "viewer-lock")))
  (:documentation "A viewer."))

(defmacro with-viewer-lock ((viewer) &body body)
  `(bt:with-lock-held ((slot-value ,viewer 'clgl::viewer-lock))
     ,@body))

(defmethod render ((viewer viewer) viewport)
  (declare (ignorable viewport))
  (with-slots (objects viewport modified) viewer
    (gl:enable :line-smooth :polygon-smooth
               :depth-test :depth-clamp)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer :depth-buffer)
    (dolist (object objects)
      (render (cdr object) viewport))))

(defun check-for-state-changes (viewer)
  (with-slots (objects to-cleanup modified should-close) viewer
    (when modified
      (dolist (object objects)
        (fill-buffers (cdr object)))
      (setf modified nil))

    (when to-cleanup
      (dolist (object to-cleanup)
        (cleanup (cdr object)))
      (setf to-cleanup nil))

    (when should-close
      (set (slot-value viewer 'window-open) nil)
      (set-window-should-close))))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun viewer-thread-function (viewer)
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (with-window (:title "OpenGL Scene Viewer"
                           :width (floor (/ cur-width 2.0f0))
                           :height (floor (/ cur-height 2.0f0))
                           :decorated t
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :resizable t)

        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (gl:clear-color 0 0 0 1.0)

        (with-viewer-lock (viewer)
          (with-slots (should-close window-open) viewer
            (setf should-close nil)
            (setf window-open t)))

        ;; The 'event loop'
        (loop until (window-should-close-p)
           do
             (with-viewer-lock (viewer)
               (check-for-state-changes viewer)
               (render viewer nil))
           do (swap-buffers)
           do (poll-events))
        (with-viewer-lock (viewer)
          (with-slots (objects) viewer
            (dolist (object objects)
              (cleanup (cdr object))))))
      )))

(defun show-viewer (viewer &optional (in-thread nil))
  (if in-thread
      (viewer-thread-function viewer)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function viewer))))

(defun set-viewport (viewer new-viewport)
  (with-viewer-lock (viewer)
    (with-slots (viewport modified) viewer
      (setf viewport new-viewport)
      )))

(defun close-viewer (viewer)
  (with-viewer-lock (viewer)
    (with-slots (window-open should-close) viewer
      (when window-open
        (set should-close t)))))

(defun add-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (objects modified) viewer
      (if-let ((item (assoc name objects)))
        (setf (cdr item) object)
        (push (cons name object) objects))
      (setf modified t))))

(defun scale-object (viewer object-name scale)
  (with-viewer-lock (viewer)
    (with-slots (objects modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmscale (slot-value (cdr items) 'transformation) (vec3 scale scale scale))
        ;; (setf modified t)
        ))))

(defun translate-object (viewer object-name offset)
  (with-viewer-lock (viewer)
    (with-slots (objects modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmtranslate (slot-value (cdr items) 'transformation) offset)
        ;; (setf modified t)
        ))))

(defun rotate-object (viewer object-name vector angle)
  (with-viewer-lock (viewer)
    (with-slots (objects modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmrotate (slot-value (cdr items) 'transformation) vector angle)
        ;; (setf modified t)
        ))))

(defun rm-object (viewer name)
  (with-viewer-lock (viewer)
    (with-slots (objects to-cleanup modified) viewer
      (when-let ((items (assoc name objects)))
        (push items to-cleanup)
        (setf objects (remove name objects :key #'car))
        (setf modified t)))))

(defun force-redraw (viewer)
  (with-viewer-lock (viewer)
    (setf (slot-value viewer 'clgl::modified) t)))

(defun clear (viewer)
  (with-viewer-lock (viewer)
    (with-slots (objects to-cleanup modified) viewer
      (setf to-cleanup objects)
      (setf objects nil)
      (setf modified t))))

