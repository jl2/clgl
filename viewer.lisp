;;;; viewer.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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
  (:documentation "A viewport and a collection of objects."))

(defmacro with-viewer-lock ((viewer) &body body)
  `(bt:with-lock-held ((slot-value ,viewer 'clgl::viewer-lock))
     ,@body))

(defmethod render ((viewer viewer) viewport)
  (declare (ignorable viewport))
  (with-slots (objects viewport modified) viewer
    (gl:enable :line-smooth
               :polygon-smooth
               :cull-face
               :depth-test :depth-clamp
               :blend)
    (gl:depth-range -10.1 10.1)
    (gl:blend-func :one :one-minus-src-alpha)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer :depth-buffer)
    (dolist (object objects)
      (render (cdr object) viewport))))

(defparameter *click-position* nil)
(defparameter *last-position* nil)
(defparameter *scrolling* nil)

(defun check-for-state-changes (viewer)
  (with-slots (objects to-cleanup modified should-close viewport) viewer
    (when *scrolling*
      (handle-scroll viewport (* 0.25 (cdr *scrolling*)))
      (setf *scrolling* nil))

    (when *click-position*
      (let* ((new-point (glfw:get-cursor-position))
             (win-size (glfw:get-window-size))
             (x-diff (/ (- (car new-point) (car *click-position*)) (car win-size)))
             (y-diff (/ (- (cadr new-point) (cadr *click-position*)) (cadr win-size))))
        (when (not (equal new-point *last-position*))
          (setf *last-position* new-point)
          (handle-mouse-drag viewport (* -1 (/ pi 8) x-diff) (* (/ pi 8) pi y-diff)))))

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
  (cond ((and (eq key :escape) (eq action :press) *click-position*)
         (setf *click-position* nil)
         (setf *last-position* nil))
        ((and (eq key :escape) (eq action :press))
         (set-window-should-close))))

(def-mouse-button-callback mouse-handler (window button action mod-keys)
  (declare (ignorable mod-keys))
  (let ((cpos (glfw:get-cursor-position window)))

    (when (and (eq button :left) (eq action :press) (null *click-position*))
      (setf *click-position* cpos)
      (setf *last-position* cpos))

    (when (and (eq button :left) (eq action :release) *click-position*)
      (setf *last-position* nil)
      (setf *click-position* nil))))

(def-scroll-callback scroll-handler (window x y)
  (declare (ignorable window))
  (setf *scrolling* (cons x y)))

(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

(defun viewer-thread-function (viewer)
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (with-window (:title "OpenGL Scene Viewer"
                           :width (/ cur-width 2)
                           :height cur-height
                           :decorated t
                           ;; :monitor monitor
                           :opengl-profile :opengl-core-profile
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-forward-compat t
                           :resizable t)
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'quit-on-escape)
        (set-error-callback 'error-callback)
        (set-mouse-button-callback 'mouse-handler)
        (set-scroll-callback 'scroll-handler)
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
              (cleanup (cdr object)))))))))

(defun show-viewer (viewer &optional (in-thread nil))
  (if in-thread
      (viewer-thread-function viewer)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function viewer))))

(defun set-viewport (viewer new-viewport)
  (with-viewer-lock (viewer)
    (with-slots (viewport modified) viewer
      (setf viewport new-viewport))))

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
        (nmscale (slot-value (cdr items) 'transformation) (vec3 scale scale scale))))))

(defun translate-object (viewer object-name offset)
  (with-viewer-lock (viewer)
    (with-slots (objects modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmtranslate (slot-value (cdr items) 'transformation) offset)))))

(defun rotate-object (viewer object-name vector angle)
  (with-viewer-lock (viewer)
    (with-slots (objects modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmrotate (slot-value (cdr items) 'transformation) vector angle)))))

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

