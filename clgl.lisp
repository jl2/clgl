;;;; clgl.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defun random-primitives (&key (points 0)
                            (lines 0)
                            (triangles 0))
  (let ((prims (make-instance 'primitives)))

    (dotimes (i points)
      (add-point prims
                 (vec3 (randr) (randr) (randr))
                 (random-color)))

    (dotimes (i lines)
      (add-line prims
                (vec3 (randr) (randr) (randr))
                (vec3 (randr) (randr) (randr))
                (random-color)))

    (dotimes (i triangles)
      (add-triangle prims
                    (vec3 (randr) (randr) (randr))
                    (vec3 (randr) (randr) (randr))
                    (vec3 (randr) (randr) (randr))
                    (random-color)))
    prims))
