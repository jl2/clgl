;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export
   #:viewport
   #:orthographic
   #:perspective
   #:get-transform
   #:apply-view-transformation
   #:2d-viewport
   #:spherical-viewport
   #:look-at-viewport

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

   #:2d-plot
   #:xy-square
   #:xz-square
   #:yz-square

   #:rotation-around-y
   #:make-parametric
   #:sphere-x
   #:sphere-y
   #:sphere-z
   #:plane-x
   #:plane-y
   #:plane-z
   #:simple-animation))

