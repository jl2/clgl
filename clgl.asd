;;;; clgl.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:clgl
  :description "A simple, REPL friendly graphics library using OpenGL."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :depends-on (#:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:bordeaux-threads
               #:trivial-main-thread
               #:3d-vectors
               #:3d-matrices
               ;; #:gpxtools
               #:kdtree
               #:stl)
  :serial t
  :components ((:file "package")
               (:file "clgl")
               (:file "opengl-object")
               (:file "primitives")
               (:file "shaders")
               (:file "viewer")
               (:file "viewport")
               (:file "transformation")
               (:file "stl-file")
               (:file "utilities")))

