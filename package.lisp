;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export
   #:viewport
   #:spherical-viewport
   #:look-at-viewport
   #:2d-viewport

   #:viewer
   #:show-viewer
   #:close-viewer
   #:add-object
   #:rm-object
   #:force-redraw
   #:set-viewport
   #:clear
   #:scale-object
   #:translate-object
   #:rotate-object

   #:primitives
   #:add-point
   #:add-line
   #:add-triangle
   #:add-filled-triangle
   #:make-3d-axis

   #:axis-viewer
   #:make-line-pattern
   #:random-primitives

   #:simple-animation))

