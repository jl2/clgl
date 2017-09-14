;;;; primitives.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass primitives (opengl-object)
  ((vertex-data :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0))
   (counts :initform nil)
   (points :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (lines :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (triangles :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (filled-triangles :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0)))
  (:documentation "A set of primitives that all use the same material."))

(defconstant +data-stride+ 10)

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
    (floor (/ olen +data-stride+))))

(defun add-point (object pt color)
  (declare (type primitives object)
           (type point pt)
           (type color color))
  (with-slots (vertex-data points) object
    (vector-push-extend (insert-point-in-buffer vertex-data pt (vec3 0.0f0 0.0f0 1.0f0) color) points)))

(defun add-line (object pt1 pt2 color)
  (declare (type primitives object)
           (type point pt1 pt2)
           (type color color))
  (with-slots (vertex-data lines) object
    (vector-push-extend (insert-point-in-buffer vertex-data pt1 (vec3 0.0f0 0.0f0 1.0f0) color) lines)
    (vector-push-extend (insert-point-in-buffer vertex-data pt2 (vec3 0.0f0 0.0f0 1.0f0) color) lines)))

(defun triangle-normal (pt1 pt2 pt3)
  "Compute the normal of a triangle."
  (declare (type point pt1 pt2 pt3))
  (vc (v- pt2 pt1) (v- pt2 pt3)))

(defun add-triangle (object pt1 pt2 pt3 color &key (filled nil) (normal nil) (norm1 nil) (norm2 nil) (norm3 nil))
  (declare (type primitives object)
           (type point pt1 pt2)
           (type color color))
  (let* ((calculated (if normal normal (triangle-normal pt1 pt2 pt3)))
         (n1 (if norm1 norm1 calculated))
         (n2 (if norm2 norm2 calculated))
         (n3 (if norm3 norm3 calculated)))
    (cond (filled
           (with-slots (vertex-data filled-triangles) object
             (vector-push-extend (insert-point-in-buffer vertex-data pt1 n1 color) filled-triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt2 n2 color) filled-triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt3 n3 color) filled-triangles)))
          (t
           (with-slots (vertex-data triangles) object
             (vector-push-extend (insert-point-in-buffer vertex-data pt1 n1 color) triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt2 n2 color) triangles)
             (vector-push-extend (insert-point-in-buffer vertex-data pt3 n3 color) triangles))))))

(defmethod fill-buffers ((object primitives))
  (call-next-method)
  (with-slots (vao vbos ebos points lines triangles filled-triangles vertex-data) object
    (when (null vbos)
      (setf vbos (gl:gen-buffers 1))
      (setf ebos (gl:gen-buffers 4)))
    (let ((gl-vertices (to-gl-float-array vertex-data)))
      
      (gl:bind-buffer :array-buffer (car vbos))
      (gl:buffer-data :array-buffer :static-draw gl-vertices)
      (gl:free-gl-array gl-vertices)
      (loop for indices in (list points lines triangles filled-triangles)
         for ebo in ebos
         do
           (when (> (length indices) 0)
             (let ((gl-indices (to-gl-array indices :unsigned-int)))
               ;; (format t "Filling VBO and EBO:~a with indices ~a~%" ebo indices)
               (gl:bind-buffer :element-array-buffer ebo)
               (gl:buffer-data :element-array-buffer :static-draw gl-indices)
               (gl:free-gl-array gl-indices)))))))
  
(defmethod render-buffers ((object primitives) viewport)
  (call-next-method)
  (with-slots (ebos points lines triangles filled-triangles) object
    (loop for ebo in ebos
       for count in (list (length points) (length lines) (length triangles) (length filled-triangles))
       for type in '(:points :lines :triangles :triangles)
       for filled in '(:line :line :line :fill)
       do
         ;; (format t "rendering ~a ~a ~a ~a~%" ebo count type filled)
         (gl:polygon-mode :front-and-back filled)
         (%gl:draw-elements type count  :unsigned-int ebo))))
