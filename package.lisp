;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export
   #:description
   #:show-scene
   #:create-scene
   #:scene
   #:viewport
   #:light
   #:box
   #:rectangle
   #:triangle
   #:line
   #:point
   #:color
   #:red
   #:green
   #:blue
   #:alpha
   #:x
   #:y
   #:z
   #:lights
   #:objects
   #:shader-input
   #:shader))

