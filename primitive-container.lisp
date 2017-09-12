
(in-package #:clgl)

(defclass primitive-container (opengl-object)
  ((vertex-data :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0))
   (points :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (lines :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (triangles :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (filled-triangles :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (points-added :initform nil)
   (lines-added :initform nil)
   (triangles-added :initform nil)
   (filled-triangles-added :initform nil))
  (:documentation "A set of primitives that all use the same material."))

(defconstant +data-stride+ 10)

(defun insert-coords-in-buffer (buffer x y z nx ny nz red green blue alpha)
  (let ((olen (length buffer)))
    (vector-push-extend (coerce x 'single-float) buffer +data-stride+)
    (vector-push-extend (coerce y 'single-float) buffer)
    (vector-push-extend (coerce z 'single-float) buffer)
    (vector-push-extend (coerce nx 'single-float) buffer)
    (vector-push-extend (coerce ny 'single-float) buffer)
    (vector-push-extend (coerce nz 'single-float) buffer)
    (vector-push-extend (coerce red 'single-float) buffer)
    (vector-push-extend (coerce green 'single-float) buffer)
    (vector-push-extend (coerce blue 'single-float) buffer)
    (vector-push-extend (coerce alpha 'single-float) buffer)
    olen))

(defun insert-point-in-buffer (buffer pt normal color)
  (let ((olen (length buffer)))
    (vector-push-extend (coerce (vx pt) 'single-float) buffer +data-stride+)
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

(defun add-point-coords (object x y z red green blue alpha)
  (with-slots (vertex-data points points-added) object
    (vector-push-extend (insert-coords-in-buffer vertex-data x y z 0.0f0 0.0f0 0.0f0 red green blue alpha) points)
    (setf points-added t)))

(defun add-line-coords (object x1 y1 z1 x2 y2 z2 red green blue alpha)
  (declare (type primitive-container object))
  (with-slots (vertex-data lines lines-added) object
    (vector-push-extend (insert-coords-in-buffer vertex-data x1 y1 z1 0.0f0 0.0f0 0.0f0 red green blue alpha) lines)
    (vector-push-extend (insert-coords-in-buffer vertex-data x2 y2 z2 0.0f0 0.0f0 0.0f0 red green blue alpha) lines)
    (setf lines-added t)))

(defun add-point (object pt color)
  (declare (type primitive-container object)
           (type point pt)
           (type color color))
  (with-slots (vertex-data points points-added) object
    (vector-push-extend (insert-point-in-buffer vertex-data pt (vec3 0.0f0 0.0f0 0.0f0) color) points)
    (setf points-added t)))

(defun add-line (object pt1 pt2 color)
  (declare (type primitive-container object)
           (type point pt1 pt2)
           (type color color))
  (with-slots (vertex-data lines lines-added) object
    (vector-push-extend (insert-point-in-buffer vertex-data pt1 (vec3 0.0f0 0.0f0 0.0f0) color) lines)
    (vector-push-extend (insert-point-in-buffer vertex-data pt2 (vec3 0.0f0 0.0f0 0.0f0) color) lines)
    (setf lines-added t)))

(defun triangle-normal (pt1 pt2 pt3)
  "Compute the normal of a triangle."
  (declare (type point pt1 pt2 pt3))
  (vc (v- pt1 pt2) (v- pt1 pt3)))

(defun add-triangle (object pt1 pt2 pt3 color &key (filled nil) (normal nil) (norm1 nil) (norm2 nil) (norm3 nil))
  (declare (type primitive-container object)
           (type point pt1 pt2)
           (type color color))
  (let* ((calculated (if normal normal (triangle-normal pt1 pt2 pt3)))
         (norm1 (if norm1 norm1 calculated))
         (norm2 (if norm2 norm2 calculated))
         (norm3 (if norm3 norm3 calculated)))
    (cond (filled
           (with-slots (vertex-data filled-triangles filled-triangles-added) object
             (vector-push-extend (insert-point-in-buffer vertex-data pt1 norm1 color) filled-triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt2 norm2 color) filled-triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt3 norm3 color) filled-triangles)
             (setf filled-triangles-added t)))
          (t
           (with-slots (vertex-data triangles triangles-added) object
             (vector-push-extend (insert-point-in-buffer vertex-data pt1 norm1 color) triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt2 norm2 color) triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt3 norm3 color) triangles)
             (setf triangles-added t))))))

(defmethod :before fill-buffers ((object primitive-container))
  (with-slots (vao vbos ebos points points-added vertex-data) object
    (when points-added 
      (when (= 0 vao)
        (setf vao (gl:gen-vertex-array)))

      (gl:bind-vertex-array vao)

      (when (null vbos)
        (setf vbos (gl:gen-buffers 1))
        (setf ebos (gl:gen-buffers 1)))

      (let ((gl-vertices (to-gl-float-array vertex-data))
            (gl-indices (to-gl-array points :unsigned-int)))
        
        (gl:bind-buffer :array-buffer (car vbos))
        (gl:buffer-data :array-buffer :static-draw gl-vertices)
        (gl:free-gl-array gl-vertices)

        (gl:bind-buffer :element-array-buffer (car ebos))
        (gl:buffer-data :element-array-buffer :static-draw gl-indices)
        (gl:free-gl-array gl-indices)))))
  
(defmethod render-buffers :after ((object primitive-container) viewport)
  (with-slots (points vbos) object
    (when (> 0 (length points))
      (%gl:draw-elements :points (length points) :unsigned-int (car vbos)))))
