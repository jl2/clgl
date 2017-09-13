;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export
   #:description
   #:create-and-view
   #:view-scene
   #:create-scene
   #:scene
   #:cleanup
   #:viewport
   #:add-point-coords
   #:add-point
   #:add-line
   #:add-triangle
   #:point
   #:add-pt
   #:force-redraw
   #:with-primitives))

