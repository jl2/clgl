;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:clgl
  (:use #:cl #:alexandria #:glfw #:3d-vectors #:3d-matrices)
  (:export
   #:viewport
   #:spherical-viewport
   #:look-at-viewport

   #:viewer
   #:show-viewer
   #:close-viewer
   #:add-object
   #:rm-object
   #:force-redraw
   #:clear

   #:primitives

   #:random-primitives))

