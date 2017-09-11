;;;; clgl.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defparameter *shader-dir* "/home/jeremiah/src/lisp/clgl/shaders/")

(defclass color ()
  ((red :initarg :red :initform 0.0f0)
   (green :initarg :green :initform 1.0f0)
   (blue :initarg :blue :initform 0.0f0)
   (alpha :initarg :alpha :initform 0.0f0))
  (:documentation "An RGB color."))

(defmethod print-object ((object color) stream)
  (with-slots (red green blue alpha) object
    (format stream 
            "(color :red ~a :green ~a :blue ~a :alpha ~a)"
            red green blue alpha)))

(defparameter *default-color* (make-instance 'color))

(defun read-file (fname)
  (with-open-file (ins fname)
    (let ((seq (make-array (file-length ins)
                           :element-type (stream-element-type ins))))
      (read-sequence seq ins) seq)))

(defclass shader-input ()
  ((name :initarg :name)
   (count :initarg :count)
   (type :initarg :type :initform :float)
   (stride :initarg :stride)
   (offset :initarg :offset)
   (attrib :initarg :attrib :initform 0)))

(defclass shader ()
  ((type :initarg :type :initform :fragment-shader)
   (text :initarg :text)
   (shader :initform 0)
   (inputs :initarg :inputs :initform nil)
   (outputs :initarg :outputs :initform nil))
  (:documentation "An OpenGL shader."))

(defclass material ()
  ((shaders :initarg :shaders :initform nil)
   (shader-program :initform nil))
  (:documentation "A combination of multiple shaders."))

(defparameter *default-materials*
  (list
   (cons
    'default
    (make-instance
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
       :outputs (list "outColor")))))))

(defclass rendered-object ()
  ((color :initarg :color :initform nil)
   (selected :initarg :selected :initform nil)
   (visible :initarg :visible :initform t)
   (regular-material :initarg :regular :initform 'default)
   (selected-material :initarg :selected :initform 'default))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defgeneric render (object)
  (:documentation "Render an object."))

(defmethod render ((object rendered-object))
  (declare (ignorable object))
  )

(defmethod print-object ((object rendered-object) stream)
  (with-slots (color selected visible regular-material selected-material) object
    (format stream "(rendered-object :color ~a :selected ~a :visible ~a :regular-material ~a :selected-material ~a)" color selected visible regular-material selected-material)))

(defgeneric prepare (object)
  (:documentation "Create and buffer OpenGL data structures for an object."))

(defmethod prepare ((object rendered-object))
  (declare (ignorable object))
  (format t "Preparing ~a~%" object)
  (finish-output t))

(defgeneric cleanup (obj)
  (:documentation "Free any OpenGL resources associated with an object."))

(defmethod cleanup ((object rendered-object))
  (declare (ignorable object))
  (format t "Cleaning up ~a~%" object))

(defclass point ()
  ((x :initarg :x :initform 0.0f0)
   (y :initarg :y :initform 0.0f0)
   (z :initarg :z :initform 0.0f0))
  (:documentation "A location in space."))

(defmethod print-object ((object point) stream)
  (with-slots (x y z) object
    (format stream "(point :x ~a :y ~a :z ~a)" x y z)))

(defclass point-set (rendered-object)
  ((points :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0)))
  (:documentation "A set of points."))

(defun add-point (object x y z red green blue alpha)
  (declare (type point-set object))
  (with-slots (points) object
    (vector-push-extend (coerce x 'single-float) points 10)
    (vector-push-extend (coerce y 'single-float) points)
    (vector-push-extend (coerce z 'single-float) points)
    (vector-push-extend 0.0f0 points)
    (vector-push-extend 0.0f0 points)
    (vector-push-extend 0.0f0 points)
    (vector-push-extend (coerce red 'single-float) points)
    (vector-push-extend (coerce green 'single-float) points)
    (vector-push-extend (coerce blue 'single-float) points)
    (vector-push-extend (coerce alpha 'single-float) points)))
  
(defclass line (rendered-object)
  ((pt1 :initarg :pt1)
   (pt2 :initarg :pt2))
  (:documentation "A line segment with an optional color."))

(defmethod print-object ((object line) stream)
  (with-slots (pt1 pt2) object
    (format stream "(line :pt1 ~a :pt2 ~a)" pt1 pt2)))

(defclass triangle (rendered-object)
  ((pt1 :initarg :pt1)
   (pt2 :initarg :pt2)
   (pt3 :initarg :pt3)
   (filled :initarg :fill :initform nil))
  (:documentation "A triangle, that may be filled, with an optional color."))

(defmethod print-object ((object triangle) stream)
  (with-slots (pt1 pt2 pt3 filled) object
    (format stream "(triangle :pt1 ~a :pt2 ~a :pt3 ~a :filled ~a)" pt1 pt2 pt3 filled)))

(defclass rectangle (rendered-object)
  ((center :initarg :pt1 :initform (make-instance 'point))
   (width :initarg :width)
   (height :initarg :height)
   (filled :initarg :fill :initform nil))
  (:documentation "A rectangle, that may be filled, with an optional color."))

(defmethod print-object ((object rectangle) stream)
  (with-slots (center width height filled) object
    (format stream "(rectangle :center ~a :width ~a :height ~a :filled ~a)" center width height filled)))

(defclass box (rendered-object)
  ((center :initarg :pt1 :initform (make-instance 'point))
   (width :initarg :width :initform 1.0f0)
   (height :initarg :height :initform 1.0f0)
   (depth :initarg :depth :initform 1.0f0)
   (color :initarg :color :initform nil))
  (:documentation "A 3D box with an optional color."))

(defmethod print-object ((object box) stream)
  (with-slots (center width height depth) object
    (format stream "(box :center ~a :width ~a :height ~a :depth ~a)" center width height depth)))

(defclass sphere (rendered-object)
  ((center :initarg :origin :initform (make-instance 'point))
   (radius :initarg :radius :initform 1.0f0))
  (:documentation "A sphere with an optional color."))

(defmethod print-object ((object sphere) stream)
  (with-slots (center radius) object
    (format stream "(sphere :center ~a :radius ~a)" center radius)))

(defclass light (rendered-object)
  ((location :initarg :location :initform (make-instance 'point :x 0.0f0 :y 10.0f0 :z 0.0f0))
   (color :initarg :color :initform (make-instance 'color :red 1.0f0 :green 1.0f0 :blue 1.0f0))))

(defmethod print-object ((object light) stream)
  (with-slots (location color) object
    (format stream "(light :location ~a :color ~a)" location color)))

(defclass viewport ()
  ((radius :initarg :radius :initform 10.0f0)
   (theta :initarg :theta :initform 0.0f0)
   (phi :initarg :phi :initform (/ pi 4))))

(defmethod print-object ((object viewport) stream)
  (with-slots (radius theta phi) object
    (format stream "(viewport :radius ~a :theta ~a phi ~a)" radius theta phi)))

(defclass scene ()
  ((viewport :initarg :viewport :initform (make-instance 'viewport))
   (lights :initarg :lights :initform (list (make-instance 'light)))
   (objects :initarg :objects :initform (list (make-instance 'box :color (make-instance 'color :red 0.0 :green 0.5 :blue 0.0))))
   (show-lights :initarg :show-lights :initform nil)
   (modified :initform nil)
   (lock :initform (bt:make-lock "scene-lock")))
  (:documentation ""))

(defmethod prepare ((object scene))
  (with-slots (objects lights viewport) object
    (format t "Preparing scene ~a~%" object)
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
  (format t "Cleaning up scene ~a~%" object))

(defmethod print-object ((object scene) stream)
  (with-slots (objects show-lights lights show-viewport viewport modified) object
    (format stream "(scene :objects ~a :show-lights ~a :lights ~a :show-viewport ~a :viewport ~a :modified ~a)" objects show-lights lights show-viewport viewport modified)))

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
           (cur-width (getf cur-mode '%cl-glfw3:width))
           (cur-height (getf cur-mode '%cl-glfw3:height)))
      (with-window (:title "OpenGL Scene Viewer"
                           :width (floor (/ cur-width 2))
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
