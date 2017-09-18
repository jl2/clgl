;;;; projection.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass projection ()
  ())

(defclass orthographic (projection)
  ((left :initarg :left  :initform -1.0)
   (right :initarg :right :initform 1.0)
   (top :initarg :top :initform 1.0)
   (bottom :initarg :bottom :initform -1.0)
   (near :initarg :near :initform 1.0)
   (far :initarg :far :initform 1000.0)))

(defmethod get-transform-matrix ((view orthographic))
  (with-slots (left right top bottom near far) view
    (mortho left right bottom top near far)))

(defclass perspective (projection)
  ((fovy :initarg :fovy :initform (* 50 (/ pi 180)))
   (aspect :initarg :aspect :initform 1.0)
   (near :initarg :near :initform 1.0)
   (far :initarg :far :initform 2000.0)))

(defmethod get-transform-matrix ((view perspective))
  (with-slots (fovy aspect near far) view
    (mperspective fovy aspect near far)))

