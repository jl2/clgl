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


(defun make-3d-axis (&optional (increment (vec3 1 1 1)) (min (vec3 -10 -10 -10)) (max (vec3 10 10 10)) (color (vec4 1.0 1.0 1.0 0.8)))
  (declare (ignorable increment min max color))
  (let* ((prims (make-instance 'clgl:primitives)))
    (clgl:add-line prims (vx__ min) (vx__ max) color)
    (clgl:add-line prims (v_y_ min) (v_y_ max) color)
    (clgl:add-line prims (v__z min) (v__z max) color)
    prims))

(defun make-line-pattern (count)
  (let* ((prims (make-instance 'clgl:primitives))
         (as-float (coerce count 'single-float)))
    (dotimes (i (+ 1 count))
      (clgl:add-line prims
                     (vec3 0.0 (/ i as-float) 0.0)
                     (vec3 (- 1.0 (/ i as-float)) 0.0 0.0)
                     (vec4 1.0 0.0 0.0 1.0))
      (clgl:add-line prims
                     (vec3 0.0 (- (/ i as-float)) 0.0)
                     (vec3 (- 1.0 (/ i as-float)) 0.0 0.0)
                     (vec4 1.0 0.0 0.0 1.0))

      (clgl:add-line prims
                     (vec3 0.0 (/ i as-float) 0.0)
                     (vec3 (+ -1.0 (/ i as-float)) 0.0 0.0)
                     (vec4 1.0 0.0 0.0 1.0))
      (clgl:add-line prims
                     (vec3 0.0 (- (/ i as-float)) 0.0)
                     (vec3 (+ -1.0 (/ i as-float)) 0.0 0.0)
                     (vec4 1.0 0.0 0.0 1.0))

      (clgl:add-line prims
                     (vec3 0.0 0.0 (/ i as-float))
                     (vec3 0.0 (- 1.0 (/ i as-float)) 0.0)
                     (vec4 1.0 1.0 0.0 1.0))
      (clgl:add-line prims
                     (vec3 0.0 0.0 (- (/ i as-float)))
                     (vec3 0.0 (- 1.0 (/ i as-float)) 0.0)
                     (vec4 1.0 1.0 0.0 1.0))

      (clgl:add-line prims
                     (vec3 0.0 0.0 (/ i as-float))
                     (vec3 0.0 (+ -1.0 (/ i as-float)) 0.0)
                     (vec4 0.0 0.0 1.0 1.0))
      (clgl:add-line prims
                     (vec3 0.0 0.0 (- (/ i as-float)))
                     (vec3 0.0 (+ -1.0 (/ i as-float)) 0.0)
                     (vec4 0.0 0.0 1.0 1.0))

      ;; (clgl:add-line prims
      ;;                (vec3 0.0 (/ i as-float) 0.0)
      ;;                (vec3 (- 1.0 (/ i as-float)) 0.0 0.0)
      ;;                (vec4 1.0 0.0 0.0 1.0))
      )
    prims))
