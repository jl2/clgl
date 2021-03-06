;;;; transformation.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defclass transformation ()
  ((matrix :initform (meye 4))))

(defgeneric get-matrix (xform))
(defmethod get-matrix ((xform transformation))
  (with-slots (matrix) xform)
  matrix)

  
