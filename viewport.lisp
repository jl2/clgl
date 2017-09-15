;;;; viewport.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defgeneric get-transform (view object-transform))
(defmethod get-transform ((view t) object-transform)
  object-transform)

(defclass spherical-viewport ()
  ((radius :initarg :radius :initform 10.0f0)
   (theta :initarg :theta :initform 0.0f0)
   (phi :initarg :phi :initform (/ pi 4))))

(defclass look-at-viewport ()
  ((eye :initarg :eye :initform (vec3 8.0f0 16.0f0 8.0f0))
   (center :initarg :center :initform (vec3 0.0f0 0.0f0 0.0f0))
   (up :initarg :up :initform +vy+)))

(defmethod get-transform ((view look-at-viewport) object-transform)
  (with-slots (eye center up) view
    (m* object-transform (mlookat eye center up))))
