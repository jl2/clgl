;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)

  (:export

   #:viewport
   #:get-transform-matrix

   #:2d-viewport
   #:look-at-viewport
   #:simple-viewport
   #:orthographic
   #:perspective
   #:rotating-viewport

   #:point-in-sphere
   #:point-on-sphere

   #:viewer
   #:show-viewer
   #:close-viewer
   #:add-object
   #:rm-object
   #:set-viewport
   #:clear

   #:force-redraw

   #:scale-object
   #:translate-object
   #:rotate-object

   #:stl-file
   
   #:primitives
   #:add-point
   #:add-line
   #:add-triangle
   #:add-filled-triangle

   #:axis-viewer

   #:make-line-pattern
   #:make-3d-axis
   #:make-2d-axis
   #:random-primitives

   #:2d-plot
   #:3d-plot
   #:plotter
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

   #:simple-animation

   #:get-matrix))

