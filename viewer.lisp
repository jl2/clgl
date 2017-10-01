;;;; viewer.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defclass viewer ()
  ((objects :initform nil :type (or null cons))
   (viewport :initarg :viewport :initform (make-instance 'look-at-viewport))

   (to-refill :initform t)
   (to-recompile :initform t)
   (to-cleanup :initform nil)

   (viewer-lock :initform (bt:make-lock "viewer-lock"))
   (window :initform nil)

   (first-click-position :initform nil)
   (last-mouse-position :initform nil)
   (should-close :initform nil))
  (:documentation "A viewport and a collection of objects."))

(defmacro with-viewer-lock ((viewer) &body body)
  `(bt:with-lock-held ((slot-value ,viewer 'clgl::viewer-lock))
     ,@body))

(defgeneric view-key-press (viewer window key scancode action mod-keys))
(defgeneric view-mouse-button (viewer window button action mod-keys))
(defgeneric view-scroll (viewer window x y))
(defgeneric view-idle (viewer window))

(defparameter *global-viewer* nil)

(defmethod render ((viewer viewer) window)
  (declare (ignorable viewer window))
  (with-slots (objects viewport) viewer
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

(defmethod view-idle ((viewer viewer) window)
  (declare (ignorable viewer window))
  (with-slots (last-position first-click-position should-close objects to-cleanup to-refill to-recompile should-close viewport) viewer

    (when first-click-position
      (let* ((new-point (glfw:get-cursor-position))
             (win-size (glfw:get-window-size))
             (x-diff (/ (- (car new-point) (car last-position)) (car win-size)))
             (y-diff (/ (- (cadr new-point) (cadr last-position)) (cadr win-size))))
        (when (not (equal new-point last-position))
          (setf last-position new-point)
          (handle-mouse-drag viewport x-diff y-diff))))

    (dolist (object to-refill)
      (fill-buffers (cdr object)))
    (setf to-refill nil)

    (dolist (object to-recompile)
      (rebuild-shaders (cdr object)))
    (setf to-recompile nil)

    (dolist (object to-cleanup)
      (cleanup (cdr object)))
    (setf to-cleanup nil)
    (when should-close
      (set-window-should-close window))))

(defmethod view-key-press ((viewer viewer) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (with-slots (last-position first-click-position objects to-cleanup geometry-modified should-close viewport) viewer
    (cond ((and (eq key :escape) (eq action :press) click-position)
           (setf click-position nil)
           (setf last-position nil))
          ((and (eq key :escape) (eq action :press))
           (set-window-should-close window)))))

(defmethod view-mouse-button ((viewer viewer) window button action mod-keys)
  (declare (ignorable window button action mod-keys))
  (with-slots (last-position first-click-position objects to-cleanup geometry-modified should-close viewport) viewer
    (let ((cpos (glfw:get-cursor-position window)))
      (cond ((and (eq button :left) (eq action :press) (null first-click-position))
             (setf first-click-position cpos)
             (setf last-position cpos))

            (first-click-position
             (let* ((new-point (glfw:get-cursor-position window))
                    (win-size (glfw:get-window-size window))
                    (x-diff (/ (- (car new-point) (car last-position)) (car win-size)))
                    (y-diff (/ (- (cadr new-point) (cadr last-position)) (cadr win-size))))
               (when (not (equal new-point last-position))
                 (setf last-position new-point)
                 (handle-mouse-drag viewport x-diff y-diff))))
            ((and (eq button :left) (eq action :release) first-click-position)
             (setf last-position nil)
             (setf first-click-position nil))))))

(defmethod  view-scroll ((viewer viewer) window x y)
  (declare (ignorable window x y))
  (with-slots (viewport) viewer
    (handle-scroll viewport (* 0.25 y))))


(def-key-callback to-global-keyboard (window key scancode action mod-keys)
  (when *global-viewer*
    (view-keyboard *global-viewer* window key scancode action mod-keys)))

(def-mouse-button-callback to-global-mouse (window button action mod-keys)
  (when *global-viewer*
    (view-mouse-button *global-viewer* window button action mod-keys)))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when *global-viewer*
    (view-keyboard *global-viewer* window key scancode action mod-keys)))

(def-scroll-callback scroll-handler (window x y)
  (when *global-viewer*
    (handle-scroll *global-viewer* window x y)))

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
                           :height (/ cur-height 2)
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

        (loop until (window-should-close-p)
           do (view-idle viewer window)
           do (render viewer window)
           do (swap-buffers)
           do (poll-events))))))

(defun show-viewer (viewer &optional (in-thread nil))
  (if in-thread
      (viewer-thread-function viewer)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function viewer))))

(defun set-viewport (viewer new-viewport)
  (with-viewer-lock (viewer)
    (with-slots (viewport) viewer
      (setf viewport new-viewport))))

(defun close-viewer (viewer)
  (with-viewer-lock (viewer)
    (with-slots (window should-close) viewer
      (when (not (zerop window))
        (set should-close t)))))

(defun add-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (objects) viewer
      (if-let ((item (assoc name objects)))
        (setf (cdr item) object)
        (push (cons name object) objects))
      (setf geometry-modified t))))

(defun scale-object (viewer object-name scale)
  (with-viewer-lock (viewer)
    (with-slots (objects geometry-modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmscale (slot-value (cdr items) 'transformation) (vec3 scale scale scale))))))

(defun translate-object (viewer object-name offset)
  (with-viewer-lock (viewer)
    (with-slots (objects geometry-modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmtranslate (slot-value (cdr items) 'transformation) offset)))))

(defun rotate-object (viewer object-name vector angle)
  (with-viewer-lock (viewer)
    (with-slots (objects geometry-modified) viewer
      (when-let ((items (assoc object-name objects)))
        (nmrotate (slot-value (cdr items) 'transformation) vector angle)))))

(defun rm-object (viewer name)
  (with-viewer-lock (viewer)
    (with-slots (objects to-cleanup geometry-modified) viewer
      (when-let ((items (assoc name objects)))
        (push items to-cleanup)
        (setf objects (remove name objects :key #'car))
        (setf geometry-modified t)))))

(defun recompile-shaders (viewer name)
  (with-viewer-lock (viewer)
    (with-slots (objects geometry-modified) viewer
      (if-let ((items (assoc name objects)))
        (progn 
          (format t "Rebuilding shader for ~a~%" items)
          (rebuild-shaders (cdr items)))
        (progn 
          (format t "Could not find shader for ~a~%" name))))))

(defun clear (viewer)
  (with-viewer-lock (viewer)
    (with-slots (objects to-cleanup) viewer
      (setf to-cleanup objects)
      (setf objects nil))))

