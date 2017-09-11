;;;; clgl.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:clgl
  :description "A simple, REPL friendly graphics library using OpenGL."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:bordeaux-threads
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "clgl")))

