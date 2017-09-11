;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw)
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

