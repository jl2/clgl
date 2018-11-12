;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)

  (:export

   #:viewport
   #:get-transform-matrix

   #:map-pt

   #:2d-viewport
   #:look-at-viewport
   #:simple-viewport
   #:orthographic
   #:perspective
   #:rotating-viewport

   #:point-in-sphere
   #:point-on-sphere

   #:viewer
   #:2d-viewer
   #:3d-viewer
   #:show-viewer
   #:close-viewer
   #:add-object
   #:rm-object
   #:recompile-shaders
   #:set-viewport
   #:clear

   #:scale-object
   #:translate-object
   #:rotate-object

   #:stl-file

   #:primitives
   #:add-point
   #:add-line
   #:add-triangle
   #:add-filled-triangle
   #:add-wire-quad
   #:add-filled-quad
   #:add-solid-box
   #:add-wire-box
   #:axis-viewer

   #:make-line-pattern
   #:make-3d-axis
   #:make-2d-axis
   #:random-primitives
   #:random-filled-triangles

   #:2d-plot
   #:3d-plot-1var
   #:3d-plot-2var
   #:3d-plot
   #:vector-plot

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

   #:get-matrix
   #:from-points
   #:from-point-list
   #:fractal-tree
   #:strange-attractor
   #:random-attractor
   #:affine-transform
   #:random-affine-transform

   #:with-viewer-lock
   #:to-rectangular))

