;;;; viewer.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defclass viewer ()
  ((objects :initform nil :type (or null cons))
   (viewport :initarg :viewport :initform (make-instance 'look-at-viewport))

   (to-refill :initform nil)
   (to-recompile :initform nil)
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

(defgeneric view-render (viewer))
(defgeneric view-key-press (viewer key scancode action mod-keys))
(defgeneric view-mouse-button (viewer button action mod-keys))
(defgeneric view-scroll (viewer x y))
(defgeneric view-idle (viewer))

(defparameter *global-viewer* nil)

(defmethod view-render ((viewer viewer))
  (declare (ignorable viewer))
  (with-viewer-lock (viewer)
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
        (render (cdr object) viewport)))))

(defmethod view-idle ((viewer viewer))
  (with-viewer-lock (viewer)
    (with-slots (last-mouse-position first-click-position should-close objects to-cleanup to-refill to-recompile viewport) viewer

      (when first-click-position
        (let* ((new-point (glfw:get-cursor-position))
               (win-size (glfw:get-window-size))
               (x-diff (/ (- (car new-point) (car last-mouse-position)) (car win-size)))
               (y-diff (/ (- (cadr new-point) (cadr last-mouse-position)) (cadr win-size))))
          (when (not (equal new-point last-mouse-position))
            (setf last-mouse-position new-point)
            (handle-mouse-drag viewport x-diff y-diff))))

      (dolist (object-name to-refill)
        (when-let (item (assoc object-name objects))
          (fill-buffers (cdr item))))
      (setf to-refill nil)

      (dolist (object-name to-recompile)
        (when-let (item (assoc object-name objects))
          (rebuild-shaders (cdr item))))
      (setf to-recompile nil)

      (dolist (object to-cleanup)
        (cleanup (cdr object)))
      (setf to-cleanup nil)

      (when should-close
        (set-window-should-close)))))

(defmethod view-key-press ((viewer viewer) key scancode action mod-keys)
  (declare (ignorable key scancode action mod-keys))
  (with-viewer-lock (viewer)
    (with-slots (last-mouse-position first-click-position objects to-cleanup geometry-modified should-close viewport) viewer
      (cond ((and (eq key :escape) (eq action :press) first-click-position)
             (setf first-click-position nil)
             (setf last-mouse-position nil))
            ((and (eq key :escape) (eq action :press))
             (set-window-should-close))))))

(defmethod view-mouse-button ((viewer viewer) button action mod-keys)
  (declare (ignorable button action mod-keys))
  (with-viewer-lock (viewer)
    (with-slots (last-mouse-position first-click-position objects to-cleanup geometry-modified should-close viewport) viewer
      (let ((cpos (glfw:get-cursor-position)))
        (cond ((and (eq button :left) (eq action :press) (null first-click-position))
               (setf first-click-position cpos)
               (setf last-mouse-position cpos))

              ((and (eq button :left) (eq action :release) first-click-position)
               (setf last-mouse-position nil)
               (setf first-click-position nil)))))))

(defmethod view-scroll ((viewer viewer) x y)
  (declare (ignorable x y))
  (with-viewer-lock (viewer)
    (with-slots (viewport) viewer
      (handle-scroll viewport x y))))

(def-key-callback to-global-keyboard (window key scancode action mod-keys)
  (declare (ignorable window))
  (when *global-viewer*
    (view-key-press *global-viewer* key scancode action mod-keys)))

(def-mouse-button-callback to-global-mouse (window button action mod-keys)
  (declare (ignorable window))
  (when *global-viewer*
    (view-mouse-button *global-viewer* button action mod-keys)))

(def-scroll-callback to-global-scroll (window x y)
  (declare (ignorable window))
  (when *global-viewer*
    (view-scroll *global-viewer* x y)))

(def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

(defun viewer-thread-function (viewer)
  (with-init
    (let* ((monitor (glfw:get-primary-monitor))
           (cur-mode (glfw:get-video-mode monitor))
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (setf *global-viewer* viewer)
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
        
        ;; (with-viewer-lock (viewer)
        ;;   (setf (slot-value viewer 'window) wind))
        (setf %gl:*gl-get-proc-address* #'get-proc-address)
        (set-key-callback 'to-global-keyboard)
        (set-error-callback 'error-callback)
        (set-mouse-button-callback 'to-global-mouse)
        (set-scroll-callback 'to-global-scroll)

        (gl:clear-color 0 0 0 1.0)

        (loop until (window-should-close-p)
           do (view-idle viewer)
           do (view-render viewer)
           do (swap-buffers)
           do (poll-events))
        (with-viewer-lock (viewer)
          (with-slots (objects) viewer
            (dolist (object objects)
              (cleanup (cdr object))))))
      (setf *global-viewer* nil))))

(defun rm-object (viewer name)
  (with-viewer-lock (viewer)
    (with-slots (objects to-cleanup geometry-modified) viewer
      (when-let ((items (assoc name objects)))
        (push items to-cleanup)
        (setf objects (remove name objects :key #'car))))))

(defun recompile-shaders (viewer name)
  (format t "Telling ~a to recompile shaders for ~a~%" viewer name)
  (with-viewer-lock (viewer)
    (with-slots (to-recompile) viewer
      (push name to-recompile))))

(defun clear (viewer)
  (with-viewer-lock (viewer)
    (with-slots (objects to-cleanup) viewer
      (setf to-cleanup objects)
      (setf objects nil))))

(defun show-viewer (viewer &optional (in-thread nil))
  (if in-thread
      (viewer-thread-function viewer)
      (trivial-main-thread:with-body-in-main-thread ()
        (viewer-thread-function viewer))))

(defun close-viewer (viewer)
  (with-viewer-lock (viewer)
    (with-slots (window should-close) viewer
      (when window
        (set should-close t)))))


(defun set-viewport (viewer new-viewport)
  (with-viewer-lock (viewer)
    (with-slots (viewport) viewer
      (setf viewport new-viewport))))

(defun add-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (to-refill to-recompile objects) viewer
      (if-let ((item (assoc name objects)))
        (setf (cdr item) object)
        (push (cons name object) objects))
      (push name to-refill)
      (push name to-recompile))))

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



