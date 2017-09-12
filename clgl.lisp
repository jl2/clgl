;;;; clgl.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defparameter *shader-dir* "/home/jeremiah/src/lisp/clgl/shaders/")

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

(defparameter *default-color* (vec 0.0f0 1.0f0 0.0f0 1.0f0))

(defun read-file (fname)
  (with-open-file (ins fname)
    (let ((seq (make-array (file-length ins)
                           :element-type (stream-element-type ins))))
      (read-sequence seq ins) seq)))

(defclass shader-input ()
  ((name :initarg :name :type string)
   (count :initarg :count :type fixnum)
   (type :initarg :type :initform :float)
   (stride :initarg :stride :type fixnum)
   (offset :initarg :offset :type fixnum)
   (attrib :initarg :attrib :initform 0 :type fixnum)))

(defclass shader ()
  ((type :initarg :type :initform :fragment-shader)
   (text :initarg :text :type string)
   (shader :initform 0 :type fixnum)
   (inputs :initarg :inputs :initform nil :type cons)
   (outputs :initarg :outputs :initform nil :type cons))
  (:documentation "An OpenGL shader."))

(defclass material ()
  ((shaders :initarg :shaders :initform nil :type cons))
  (:documentation "A combination of multiple shaders."))

;; (defun compile-and-link (material)
;;   "Load and compile a shader and print any compiler messages."
;;   (with-slots (shaders) material
;;     (dolist (shader shaders)
;;       (with-slots (text type shader) shader
;;         (setf shader (gl:create-shader type))
;;         (gl:shader-source shader (list text))
;;         (gl:compile-shader shader)
;;         (when (not (eq t (gl:get-shader shader :compile-status)))
;;           (format t "compile-status: ~a~%" (gl:get-shader shader :compile-status))
;;           (format t "info-log ~a~%" (gl:get-shader-info-log shader)))))

;;       (dolist (shader shaders)
;;         (gl:attach-shader shader-program (shader-shader shader))
;;         (loop
;;            for output in (shader-outputs shader)
;;            for idx from 0
;;            do (gl:bind-frag-data-location shader-program idx output)))
;; (gl:link-program shader-program)
;;       (let ((status (gl:get-program shader-program :link-status)))
;;         (format t "link-program: ~a~%~a~%" status(gl:get-program-info-log shader-program)))

;;       (gl:validate-program shader-program)
;;       (let ((status (gl:get-program shader-program :validate-status)))
;;         (format t "validate-program: ~a~%~a~%" status (gl:get-program-info-log shader-program)))

;;       (gl:use-program shader-program)


(defmethod cleanup ((obj shader))
  "Delete a shader on the GPU."
  (with-slots (shader) obj
    (when (> 0 shader)
      (gl:delete-shader shader))))

(defparameter *default-material-list*
  (list (make-instance
         'material
         :shaders
         (list
          (make-instance
           'shader
           :type :vertex-shader
           :text (read-file (merge-pathnames *shader-dir* "default-vertex.glsl"))
           :inputs
           (list
            (make-instance
             'shader-input
             :name "position"
             :type :float
             :count 3
             :stride (* (+ 3 3 4) (cffi:foreign-type-size :float))
             :offset 0)
            (make-instance
             'shader-input :name "normal"
             :type :float
             :count 3
             :stride (* (+ 3 3 4) (cffi:foreign-type-size :float))
             :offset (* 3 (cffi:foreign-type-size :float)))
            (make-instance
             'shader-input :name "color"
             :type :float
             :count 4
             :stride (* (+ 3 3 4) (cffi:foreign-type-size :float))
             :offset (* 6 (cffi:foreign-type-size :float)))))
          (make-instance
           'shader :type :fragment-shader
           :text (read-file (merge-pathnames *shader-dir* "default-frag.glsl"))
           :outputs (list "outColor"))))))

(defclass opengl-object ()
  ((color :initarg :color :initform nil :type '(or null color))
   (selected :initarg :selected :initform nil :type t)
   (visible :initarg :visible :initform t :type t)
   (regular-material-id :initarg :regular :initform 0 :type fixnum)
   (selected-material-id :initarg :selected :initform 0 :type fixnum)
   (vao :initform 0 :type fixnum)
   (vbos :initform nil :type cons)
   (shader-program :initform nil :type fixnum)
   (data-buffer :initform nil))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric render (object)
  (:documentation "Render an object."))

(defmethod render ((object opengl-object))
  (declare (ignorable object)))

(defgeneric rebuild-buffers (object)
  (:documentation "Create and buffer OpenGL data structures for an object."))

(defmethod rebuild-buffers ((object opengl-object))
  (declare (ignorable object))
  (format t "Preparing opengl-object for rendering~%")
  (finish-output t))

(defgeneric cleanup (obj)
  (:documentation "Free any OpenGL resources associated with an object."))

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
      (gl:delete-vertex-arrays (list vao))))
  (call-next-method))

(defclass point-set (opengl-object)
  ((points :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0)))
  (:documentation "A set of points."))

(defun insert-small-coords-in-buffer (buffer x y z red green blue alpha)
  (let ((olen (length buffer)))
    (vector-push-extend (coerce x 'single-float) buffer 10)
    (vector-push-extend (coerce y 'single-float) buffer)
    (vector-push-extend (coerce z 'single-float) buffer)
    (vector-push-extend 0.0f0 buffer)
    (vector-push-extend 0.0f0 buffer)
    (vector-push-extend 0.0f0 buffer)
    (vector-push-extend (coerce red 'single-float) buffer)
    (vector-push-extend (coerce green 'single-float) buffer)
    (vector-push-extend (coerce blue 'single-float) buffer)
    (vector-push-extend (coerce alpha 'single-float) buffer)
    olen))

(defun insert-coords-in-buffer (buffer x y z nx ny nz red green blue alpha)
  (let ((olen (length buffer)))
    (vector-push-extend (coerce x 'single-float) buffer 10)
    (vector-push-extend (coerce y 'single-float) buffer)
    (vector-push-extend (coerce z 'single-float) buffer)
    (vector-push-extend nx buffer)
    (vector-push-extend ny buffer)
    (vector-push-extend nz buffer)
    (vector-push-extend (coerce red 'single-float) buffer)
    (vector-push-extend (coerce green 'single-float) buffer)
    (vector-push-extend (coerce blue 'single-float) buffer)
    (vector-push-extend (coerce alpha 'single-float) buffer)
    olen))

(defun insert-point-in-buffer (buffer pt normal color)
  (let ((olen (length buffer)))
    (vector-push-extend (coerce (vx pt) 'single-float) buffer 10)
    (vector-push-extend (coerce (vy pt) 'single-float) buffer)
    (vector-push-extend (coerce (vz pt) 'single-float) buffer)
    (vector-push-extend (coerce (vx normal) 'single-float) buffer)
    (vector-push-extend (coerce (vy normal) 'single-float) buffer)
    (vector-push-extend (coerce (vz normal) 'single-float) buffer)
    (vector-push-extend (coerce (red color) 'single-float) buffer)
    (vector-push-extend (coerce (green color) 'single-float) buffer)
    (vector-push-extend (coerce (blue color) 'single-float) buffer)
    (vector-push-extend (coerce (alpha color) 'single-float) buffer)
    olen))

(defun add-coords (object x y z red green blue alpha)
  (declare (type point-set object))
  (with-slots (points) object
    (insert-small-coords-in-buffer points x y z red green blue alpha)))

(defun add-point (object pt color)
  (declare (type point-set object)
           (type point pt)
           (type color color))
  (with-slots (points) object
    (insert-point-in-buffer points pt (vec3 0.0f0 0.0f0 0.0f0) color)))


(defun triangle-normal (pt1 pt2 pt3)
  "Compute the normal of a triangle."
  (declare (type point pt1 pt2 pt3))
  (vc (v- pt1 pt2) (v- pt1 pt3)))

(defun add-triangle (object pt1 pt2 pt3 color &optional (normal nil) (norm1 nil) (norm2 nil) (norm3 nil))
  (declare (type point-set object))
  (let* ((calculated (if normal normal (triangle-normal pt1 pt2 pt3)))
         (norm1 (if norm1 norm1 calculated))
         (norm2 (if norm2 norm2 calculated))
         (norm3 (if norm3 norm3 calculated)))
    (with-slots (triangles) object
      (insert-point-in-buffer triangles pt1 norm1 color)
      (insert-point-in-buffer triangles pt2 norm2 color)
      (insert-point-in-buffer triangles pt3 norm3 color))))

(defclass line (opengl-object)
  ((pt1 :initarg :pt1)
   (pt2 :initarg :pt2))
  (:documentation "A line segment with an optional color."))

(defclass line-set (opengl-object)
  ((triangles :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0))
   (filled :initarg :fill :initform nil))
  (:documentation "A set of lines."))
  
(defclass triangle (opengl-object)
  ((pt1 :initarg :pt1)
   (pt2 :initarg :pt2)
   (pt3 :initarg :pt3)
   (filled :initarg :fill :initform nil))
  (:documentation "A triangle, that may be filled, with an optional color."))

(defclass triangle-set (opengl-object)
  ((triangles :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0))
   (filled :initarg :fill :initform nil))
  (:documentation "A set of triangles."))

(defclass rectangle (opengl-object)
  ((center :initarg :pt1 :initform (vec3 0.0f0 0.0f0 0.0f0))
   (width :initarg :width :initform 1.0f0)
   (height :initarg :height :initform 1.0f0)
   (filled :initarg :fill :initform nil))
  (:documentation "A rectangle, that may be filled, with an optional color."))

(defclass box (opengl-object)
  ((center :initarg :pt1 :initform (vec3 0.0f0 0.0f0 0.0f0))
   (width :initarg :width :initform 1.0f0)
   (height :initarg :height :initform 1.0f0)
   (depth :initarg :depth :initform 1.0f0)
   (color :initarg :color :initform nil))
  (:documentation "A 3D box with an optional color."))

(defclass sphere (opengl-object)
  ((center :initarg :origin :initform (vec3 0.0f0 0.0f0 0.0f0))
   (radius :initarg :radius :initform 1.0f0))
  (:documentation "A sphere with an optional color."))

(defclass light (opengl-object)
  ((location :initarg :location :initform (make-instance 'point :x 0.0f0 :y 10.0f0 :z 0.0f0))
   (color :initarg :color :initform (make-instance 'color :red 1.0f0 :green 1.0f0 :blue 1.0f0))))

(defclass viewport ()
  ((radius :initarg :radius :initform 10.0f0)
   (theta :initarg :theta :initform 0.0f0)
   (phi :initarg :phi :initform (/ pi 4))))

(defclass scene ()
  ((viewport :initarg :viewport :initform (make-instance 'viewport))
   (lights :initarg :lights :initform (list (make-instance 'light)))
   (objects :initarg :objects :initform (list (make-instance 'box :color (make-instance 'color :red 0.0 :green 0.5 :blue 0.0))))
   (show-lights :initarg :show-lights :initform nil)
   (modified :initform nil)
   (materials :initform (make-array
                         1
                         :element-type 'material :initial-contents *default-material-list*))
   (lock :initform (bt:make-lock "scene-lock")))
  (:documentation ""))

(defmethod prepare ((object scene))
  (with-slots (objects lights viewport) object
    (format t "Preparing scene~%")
    (dolist (light lights)
      (prepare light))
    (dolist (object objects)
      (prepare object))))

(defmethod render ((object scene))
  (declare (ignorable object))
  (with-slots (objects show-lights lights viewport modified) object
    (when modified
      (format t "Scene was modified~%")
      (prepare object)
      (setf modified nil))
    (dolist (object objects)
      (render object))
    (when show-lights
      (dolist (light lights)
        (render light)))))

(defmethod cleanup ((object scene))
  (format t "Cleaning up scene~%")
  (with-slots (objects show-lights lights viewport modified) object
    (dolist (obj objects)
      (cleanup obj))
    (dolist (light lights)
      (cleanup light)))
  (call-next-method))

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
        (gl:clear-color 0 0 0 0)

        ;; The 'event loop'
        (loop until (window-should-close-p)
           do
             (bt:with-lock-held ((slot-value scene 'lock))
               (render scene))
           do (swap-buffers)
           do (poll-events))
        ;; Finally clean up
        (cleanup scene)))))

(defun show-scene (scene &optional (in-main-thread t))
  (if in-main-thread
      (trivial-main-thread:with-body-in-main-thread ()
        (scene-viewer scene))
      (scene-viewer scene)))
