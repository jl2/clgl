;;;; clgl.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass renderable-object ()
  )

(defclass point (renderable-object)
  )

(defclass line (renderable-object)
  )

(defclass triangle (renderable-object)
  )

(defclass rectangle (renderable-object)
  )


(defclass cube (renderable-object)
  )

(defclass sphere (renderable-object)
  )


(defclass shader ()
  )

(defclass light ()
  )

(defclass viewport ()
  )

(defclass scene ()
  )

