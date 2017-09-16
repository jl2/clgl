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


(defun make-3d-axis (&key
                       (increment (vec3 1 1 1))
                       (lower (vec3 -1 -1 -1))
                       (upper (vec3 1 1 1))
                       (color (vec4 1.0 1.0 1.0 1.0)))
  (declare (ignorable increment))
  (let* ((prims (make-instance 'clgl:primitives)))
    (clgl:add-line prims (vx__ lower) (vx__ upper) color)
    (clgl:add-line prims (v_y_ lower) (v_y_ upper) color)
    (clgl:add-line prims (v__z lower) (v__z upper) color)
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


(defun axis-viewer (&optional in-thread)
  (let ((viewer (make-instance 'viewer)))
    (add-object viewer 'axis (make-3d-axis))
    (add-object viewer 'xysquare (clgl:xy-square))
    (add-object viewer 'yzsquare (clgl:yz-square))
    (show-viewer viewer in-thread)
    viewer))

(defun 2d-plot (&key
                  (yf #'sin)
                  (xf #'identity)
                  (min-t (- pi))
                  (max-t pi)
                  (steps 100)
                  (color (vec4 1.0 1.0 1.0 1.0)))
  (let ((prims (make-instance 'primitives))
        (dt (/ (- max-t min-t) 1.0 steps)))
    (dotimes (i (1- steps))
      (let ((t-value (+ min-t (* dt i)))
            (next-t-value (+ min-t (* dt i))))
        (add-line prims
                  (vec (funcall xf t-value)
                       (funcall yf t-value)
                       0)
                  (vec (funcall xf next-t-value)
                       (funcall yf next-t-value)
                       0)
                  color)))
    prims))

(defmacro simple-animation ((variable steps) &body body)
  `(dotimes (,variable (1+ ,steps))
     ,@body
     (sleep (/ 1.0 30.0))))

(defun xy-square ()
  (let ((square (make-instance 'clgl:primitives)))
    (add-line square (vec3 0 0 0) (vec3 1 0 0) (vec4 1 0 0 1))
    (add-line square (vec3 1 0 0) (vec3 1 1 0) (vec4 0 1 0 1))
    (add-line square (vec3 1 1 0) (vec3 0 1 0) (vec4 0 0 1 1))
    (add-line square (vec3 0 1 0) (vec3 0 0 0) (vec4 1 0 1 1))
    square))

(defun xz-square ()
  (let ((square (make-instance 'clgl:primitives)))
    (add-line square (vec3 0 0 0) (vec3 1 0 0) (vec4 1 0 0 1))
    (add-line square (vec3 1 0 0) (vec3 1 0 1) (vec4 0 1 0 1))
    (add-line square (vec3 1 0 1) (vec3 0 0 1) (vec4 0 0 1 1))
    (add-line square (vec3 0 0 1) (vec3 0 0 0) (vec4 1 0 1 1))
    square))

(defun yz-square ()
  (let ((square (make-instance 'clgl:primitives)))
    (add-line square (vec3 0 0 0) (vec3 0 1 0) (vec4 1 0 0 1))
    (add-line square (vec3 0 1 0) (vec3 0 1 1) (vec4 0 1 0 1))
    (add-line square (vec3 0 1 1) (vec3 0 0 1) (vec4 0 0 1 1))
    (add-line square (vec3 0 0 1) (vec3 0 0 0) (vec4 1 0 1 1))
    square))
