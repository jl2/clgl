;;;; primitives.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass primitives (opengl-object)
  ((line-vertex-data :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0))
   (filled-vertex-data :initform (make-array 0 :element-type 'single-float :initial-contents '() :adjustable t :fill-pointer 0))
   (points :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (lines :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (triangles :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0))
   (filled-triangles :initform (make-array 0 :element-type 'fixnum :initial-contents '() :adjustable t :fill-pointer 0)))
  (:documentation "A set of primitives that all use the same material."))

(defun point-ebo (ebos)
  (car ebos))

(defun line-ebo (ebos)
  (cadr ebos))

(defun triangle-ebo (ebos)
  (caddr ebos))

(defun filled-ebo (ebos)
  (cadddr ebos))

(defconstant +line-data-stride+ 7)
(defun insert-pc-in-buffer (buffer pt color)
  (let ((olen (length buffer)))
    (vector-push-extend (vx pt) buffer)
    (vector-push-extend (vy pt) buffer)
    (vector-push-extend (vz pt) buffer)
    (vector-push-extend (red color) buffer)
    (vector-push-extend (green color) buffer)
    (vector-push-extend (blue color) buffer)
    (vector-push-extend (alpha color) buffer)
    (floor (/ olen +line-data-stride+))))

(defconstant +filled-data-stride+ 10)
(defun insert-pnc-in-buffer (buffer pt normal color)
  (let ((olen (length buffer)))
    (vector-push-extend (vx pt) buffer)
    (vector-push-extend (vy pt) buffer)
    (vector-push-extend (vz pt) buffer)
    (vector-push-extend (vx normal) buffer)
    (vector-push-extend (vy normal) buffer)
    (vector-push-extend (vz normal) buffer)
    (vector-push-extend (red color) buffer)
    (vector-push-extend (green color) buffer)
    (vector-push-extend (blue color) buffer)
    (vector-push-extend (alpha color) buffer)
    (floor (/ olen +filled-data-stride+))))

(defun add-point (object pt color)
  (declare (type primitives object)
           (type point pt)
           (type color color))
  (with-slots (line-vertex-data points) object
    (vector-push-extend (insert-pc-in-buffer line-vertex-data
                                             pt
                                             color)
                        points)))

(defun add-line (object pt1 pt2 color)
  (declare (type primitives object)
           (type point pt1 pt2)
           (type color color))
  (with-slots (line-vertex-data lines) object
    (vector-push-extend (insert-pc-in-buffer line-vertex-data
                                                pt1
                                                color)
                        lines)
    (vector-push-extend (insert-pc-in-buffer line-vertex-data
                                                pt2
                                                color)
                        lines)))

(defun triangle-normal (pt1 pt2 pt3)
  "Compute the normal of a triangle."
  (declare (type point pt1 pt2 pt3))
  (vc (v- pt1 pt2) (v- pt3 pt1)))

(defun add-triangle (object pt1 pt2 pt3 color)
  (declare (type primitives object)
           (type point pt1 pt2)
           (type color color))
  (with-slots (filled-vertex-data triangles) object
    (vector-push-extend (insert-pc-in-buffer filled-vertex-data
                                             pt1
                                             color)
                        triangles)
    (vector-push-extend (insert-pc-in-buffer filled-vertex-data
                                             pt2
                                             color)
                        triangles)
    (vector-push-extend (insert-pc-in-buffer filled-vertex-data
                                             pt3
                                             color)
                        triangles)))

(defun add-filled-triangle (object pt1 pt2 pt3 color)

  (declare (type primitives object)
           (type point pt1 pt2)
           (type color color))

  (let ((normal (triangle-normal pt1 pt2 pt3)))
    (with-slots (filled-vertex-data filled-triangles) object
      (vector-push-extend (insert-pnc-in-buffer filled-vertex-data
                                                  pt1
                                                  normal
                                                  color)
                          filled-triangles)
      (vector-push-extend (insert-pnc-in-buffer filled-vertex-data
                                                  pt2
                                                  normal
                                                  color)
                          filled-triangles)
      (vector-push-extend (insert-pnc-in-buffer filled-vertex-data
                                                  pt3
                                                  normal
                                                  color)
                          filled-triangles))))


(defmethod fill-buffers ((object primitives))
  (call-next-method)
  (with-slots (vao vbos ebos
                   points lines
                   triangles filled-triangles
                   line-vertex-data filled-vertex-data) object
    (when (null vbos)
      (setf vbos (gl:gen-buffers 2))
      (setf ebos (gl:gen-buffers 4)))

    (let ((gl-vertices (to-gl-float-array line-vertex-data)))
      (gl:bind-buffer :array-buffer (car vbos))
      (gl:buffer-data :array-buffer :static-draw gl-vertices)
      (gl:free-gl-array gl-vertices))

    (let ((gl-vertices (to-gl-float-array filled-vertex-data)))
      (gl:bind-buffer :array-buffer (cadr vbos))
      (gl:buffer-data :array-buffer :static-draw gl-vertices)
      (gl:free-gl-array gl-vertices))

    (loop for indices in (list points lines triangles filled-triangles)
       for ebo in ebos
       do
         (let ((gl-indices (to-gl-array indices :unsigned-int)))
           (format t "Filling VBO and EBO:~a with indices ~a~%" ebo indices)
           (gl:bind-buffer :element-array-buffer ebo)
           (gl:buffer-data :element-array-buffer :static-draw gl-indices)
           (gl:free-gl-array gl-indices)))))
  
(defmethod render ((object primitives) viewport)
  (call-next-method)
  (with-slots (vbos ebos points lines triangles filled-triangles shader-programs) object

    (gl:polygon-mode :front-and-back :line)
    (gl:bind-buffer :array-buffer (car vbos))
    (use-program (car shader-programs))
    (when (> (length points) 0)
      (gl:bind-buffer :element-array-buffer (point-ebo ebos))
      (gl:draw-elements :points (gl:make-null-gl-array :unsigned-int) :count (length points)))

    (when (> (length lines) 0)
      (gl:bind-buffer :element-array-buffer (line-ebo ebos))
      (gl:draw-elements :lines (gl:make-null-gl-array :unsigned-int) :count (length lines)))

    (when (> (length triangles) 0)
      (gl:bind-buffer :array-buffer (cadr vbos))
      (gl:bind-buffer :element-array-buffer (triangle-ebo ebos))
      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length triangles)))

    (when (> (length filled-triangles) 0)
      (gl:bind-buffer :array-buffer (cadr vbos))
      (use-program (cadr shader-programs))
      (gl:polygon-mode :front-and-back :fill)
      (gl:bind-buffer :element-array-buffer (filled-ebo ebos))
      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count (length filled-triangles)))
    ))
    
