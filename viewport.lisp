;;;; viewport.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass viewport ()
  ())

(defgeneric get-transform-matrix (view))
(defmethod get-transform-matrix (view)
  (meye 4))

(defclass 2d-viewport (viewport)
  ((radius :initarg :radius :initform 10.0)))

(defmethod get-transform-matrix ((view 2d-viewport))
  (with-slots (radius) view
    (let ((nradisu (- radius)))
      (mortho nradius radius
              nradius radius
              nradius radius))))


(defclass look-at-viewport (viewport)
  ((eye :initarg :eye :initform (vec3 16.0f0 16.0f0 16.0f0))
   (center :initarg :center :initform (vec3 0.0f0 0.0f0 0.0f0))
   (up :initarg :up :initform +vy+)))

(defmethod get-transform-matrix ((view look-at-viewport))
  (with-slots (eye center up) view
    (let ((ilen (/ 1 (vlength (v- eye center))))
          (return-value (mlookat center eye up)))
      (m* (mscaling (vec3 ilen ilen ilen)) return-value))))


(defclass simple-viewport (viewport)
  ((distance :initform 10 :initarg :distance)))

(defmethod get-transform-matrix ((view simple-viewport))
  (with-slots (distance) view
    (m* (mperspective 50 1.0 1.0 1000.0) (mtranslation (vec3 0 0 (- distance))))))


(defclass orthographic (viewport)
  ((left :initarg :left  :initform -1.0)
   (right :initarg :right :initform 1.0)
   (top :initarg :top :initform 1.0)
   (bottom :initarg :bottom :initform -1.0)
   (near :initarg :near :initform 0.00001)
   (far :initarg :far :initform 1000.0)))

(defmethod get-transform-matrix ((view orthographic))
  (with-slots (left right top bottom near far) view
    (mortho left right bottom top far near)))

(defclass perspective (viewport)
  ((fovy :initarg :fovy :initform 50)
   (aspect :initarg :aspect :initform 1.0)
   (near :initarg :near :initform 1.0)
   (far :initarg :far :initform 200.0)))

(defmethod get-transform-matrix ((view perspective))
  (with-slots (fovy aspect near far) view
    (mperspective fovy aspect near far)))

