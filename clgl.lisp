;;;; clgl.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:clgl)

(defun random-primitives (&key (points 0) (lines 0) (triangles 0))

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

(defun random-filled-triangles (count)
  (let ((prims (make-instance 'primitives)))
    (flet ((random-function ()
             (randr :min -10.0 :max 10.0)))
    (dotimes (i count)
      (add-filled-triangle prims
                    (vec3 (random-function) (random-function) 0.0)
                    (vec3 (random-function) (random-function) 0.0)
                    (vec3 (random-function) (random-function) 0.0)
                    (random-color)))
    prims)))


(defun make-3d-axis (&key (lower (vec3 -1 -1 -1)) (upper (vec3 1 1 1)))
  (let* ((prims (make-instance 'clgl:primitives)))
    (clgl:add-line prims (vx__ lower) (vx__ upper) (vec4 1 0 0 1))
    (clgl:add-line prims (v_y_ lower) (v_y_ upper) (vec4 0 1 0 1))
    (clgl:add-line prims (v__z lower) (v__z upper) (vec4 0 0 1 1))
    prims))

(defun make-2d-axis (&key (lower (vec3 -1 -1 0)) (upper (vec3 1 1 0)))
  (let* ((prims (make-instance 'clgl:primitives)))
    (clgl:add-line prims (vx__ lower) (vx__ upper) (vec4 1 0 0 1))
    (clgl:add-line prims (v_y_ lower) (v_y_ upper) (vec4 0 0 1 1))
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
                     (vec4 0.0 0.0 1.0 1.0)))
    prims))

(defun axis-viewer (&key (name 'object) (object nil) (show t) (in-thread nil))
  (let ((viewer (make-instance 'viewer :viewport (make-instance 'clgl:rotating-viewport :radius 4))))
    (add-object viewer 'axis (make-3d-axis))
    (when object
      (add-object viewer name object ))
    (when show
      (show-viewer viewer in-thread))
    viewer))

(defun 2d-plot (&key
                  (yf #'sin)
                  (xf #'identity)
                  (min-t (- pi))
                  (max-t pi)
                  (steps 100)
                  (color (vec4 0.0 1.0 0.0 1.0)))
  (let ((prims (make-instance 'primitives))
        (dt (/ (- max-t min-t) 1.0 steps)))
    (dotimes (i (1- steps))
      (let ((t-value (+ min-t (* dt i)))
            (next-t-value (+ min-t (* dt (+ i 1)))))
        (add-line prims
                  (vec (funcall xf t-value)
                       (funcall yf t-value)
                       0)
                  (vec (funcall xf next-t-value)
                       (funcall yf next-t-value)
                       0)
                  color)))
    prims))

(defun 3d-plot (&key
                  (yf #'sin)
                  (xf #'identity)
                  (zf #'cos)
                  (min-t (- pi))
                  (max-t pi)
                  (steps 100)
                  (color (vec4 0.0 1.0 0.0 1.0)))
  (let ((prims (make-instance 'primitives))
        (dt (/ (- max-t min-t) 1.0 steps)))
    (dotimes (i (1- steps))
      (let ((t-value (+ min-t (* dt i)))
            (next-t-value (+ min-t (* dt (+ i 1)))))
        (add-line prims
                  (vec (funcall xf t-value)
                       (funcall yf t-value)
                       (funcall zf t-value))
                  (vec (funcall xf next-t-value)
                       (funcall yf next-t-value)
                       (funcall zf next-t-value))
                  color)))
    prims))

(defun vector-plot (&key
                      (vfun (lambda (pt)
                              (vec3 (sin (vx pt))
                                    (cos (vy pt))
                                    (vz pt))))
                      (min-t (vec3 (- pi) (- pi) (- pi)))
                      (max-t (vec3 pi pi pi))
                      (steps 200)
                      (color (vec4 0.0 1.0 0.0 1.0)))
  (let ((prims (make-instance 'primitives))
        (dt (v/ (v- max-t min-t) (vec3 steps steps steps) 1.0)))
    (dotimes (i (1- steps))
      (let* ((i-vec (vec3 i i i))
            (t-value (v+ min-t (v* dt i-vec)))
            (next-t-value (v+ min-t (v* dt (v+ i-vec (vec3 1 1 1))))))
        (add-line prims
                  (funcall vfun t-value)
                  (funcall vfun next-t-value)
                  color)))
    prims))

(defmacro simple-animation ((variable duration) &body body)
  `(dotimes (,variable (ceiling (1+ (* 30 ,duration))))
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

(defun sphere-x (uv vv)
  (declare (ignorable uv vv))
  (* (sin uv) (cos vv)))

(defun sphere-y (uv vv)
  (declare (ignorable uv vv))
  (* (cos uv) (cos vv)))

(defun sphere-z (uv vv)
  (declare (ignorable uv vv))
  (sin vv))

(defun plane-x (uv vv)
  (declare (ignorable uv vv))
  uv)

(defun plane-y (uv vv)
  (declare (ignorable uv vv))
  vv)

(defun plane-z (uv vv)
  (declare (ignorable uv vv))
  0.0)


(defun add-quad (obj color xf yf zf uv vv nu nv filled)
  (let ((tri-function (if filled #'add-filled-triangle #'add-triangle)))
    (funcall tri-function obj
             (vec3 (funcall xf nu vv)
                   (funcall yf nu vv)
                   (funcall zf nu vv))

             (vec3 (funcall xf nu nv)
                   (funcall yf nu nv)
                   (funcall zf nu nv))

             (vec3 (funcall xf uv nv)
                   (funcall yf uv nv)
                   (funcall zf uv nv))
             color)

    (funcall tri-function obj

             (vec3 (funcall xf uv vv)
                   (funcall yf uv vv)
                   (funcall zf uv vv))

             (vec3 (funcall xf uv nv)
                   (funcall yf uv nv)
                   (funcall zf uv nv))

             (vec3 (funcall xf nu vv)
                   (funcall yf nu vv)
                   (funcall zf nu vv))
             color)))


(defun add-quad (obj color pt1 pt2 pt3 pt4 filled)
  (let ((tri-function (if filled #'add-filled-triangle #'add-triangle)))
    (funcall tri-function obj
             (vec3 (funcall xf nu vv)
                   (funcall yf nu vv)
                   (funcall zf nu vv))

             (vec3 (funcall xf nu nv)
                   (funcall yf nu nv)
                   (funcall zf nu nv))

             (vec3 (funcall xf uv nv)
                   (funcall yf uv nv)
                   (funcall zf uv nv))
             color)

    (funcall tri-function obj

             (vec3 (funcall xf uv vv)
                   (funcall yf uv vv)
                   (funcall zf uv vv))

             (vec3 (funcall xf uv nv)
                   (funcall yf uv nv)
                   (funcall zf uv nv))

             (vec3 (funcall xf nu vv)
                   (funcall yf nu vv)
                   (funcall zf nu vv))
             color)))

(defun add-box (obj color
                pt1 pt2 pt3 pt4
                pt5 pt6 pt7 pt8
                filled)
  (cond (filled
         (add-filled-triangle obj pt1 pt2 pt3 color)
         (add-filled-triangle obj pt2 pt3 pt4 color)
         (add-filled-triangle obj pt5 pt6 pt7 color)
         (add-filled-triangle obj pt6 pt7 pt8 color))
        (t 
         (add-triangle obj pt1 pt2 pt3 color)
         (add-triangle obj pt2 pt3 pt4 color)
         (add-triangle obj pt5 pt6 pt7 color)
         (add-triangle obj pt6 pt7 pt8 color))))


(defun make-parametric (&key
                          (xf #'sphere-x)
                          (yf #'sphere-y)
                          (zf #'sphere-z)
                          (u-steps 20)
                          (v-steps 20)
                          (filled nil)
                          (u-min 0.0) (u-max pi)
                          (v-min 0.0) (v-max (* 2 pi))
                          (color (vec4 0 1 0 1)))
  (let ((object (make-instance 'clgl:primitives))
        (du (/ (- u-max u-min) u-steps))
        (dv (/ (- v-max v-min)  (1- v-steps))))
    (dotimes (i u-steps)
      (dotimes (j v-steps)
        (let ((uv (+ u-min (* i du)))
              (vv (+ v-min (* j dv)))
              (nu (+ u-min (* (1+ i) du)))
              (nv (+ v-min (* (1+ j) dv))))
          (add-quad object color
                    xf yf zf
                    uv vv nu nv
                    filled))))
    object))

(defun rotation-around-y (viewer &optional (radius 20) (duration 2))
  (clgl:simple-animation (i duration)
    (clgl:set-viewport
     viewer
     (make-instance 'clgl:look-at-viewport :up +vy+
                    :eye (vec3 (* radius (cos (* i (/ pi 180))))
                               (/ radius 2)
                               (* radius (sin (* i (/ pi 180)))))))))

(defun plotter (&optional (in-thread nil))
  (let ((viewer (make-instance 'clgl:viewer :viewport (make-instance 'clgl:2d-viewport))))
    (add-object viewer 'axis (make-2d-axis))
    (add-object viewer 'plot (2d-plot :xf (lambda (tv) (* 3.0 (cos (* 4 tv)) (sin tv)))
                                      :yf (lambda (tv) (* 3.0 (cos (* 4 tv)) (cos tv)))))
    (show-viewer viewer in-thread)
    viewer))


(defgeneric bounding-box (points))

(defmethod bounding-box ((points cons))
  (loop for pt in points
     minimizing (vx pt) into min-x
     maximizing (vx pt) into max-x
     minimizing (vy pt) into min-y
     maximizing (vy pt) into max-y
     minimizing (vz pt) into min-z
     maximizing (vz pt) into max-z
     finally (return (values (vec3 min-x min-y min-z) (vec3 max-x max-y max-z)))))

(defun map-pt (val min-val max-val)
  (let ((tmp (v- (v* 
                  (v/ (v- val min-val)
                      (v- max-val min-val)))
                 (vec3 0.5 0.5 0.5))))
    (vec3 (vx tmp) (- (vy tmp)) (vz val))))

(defmethod bounding-box ((points kdtree:kd-tree-node))
  (let* ((min-point (kdtree:kd-tree-node-pt points))
         (max-point (kdtree:kd-tree-node-pt points))
         (min-x (vx min-point))
         (max-x (vx max-point))
         (min-y (vy min-point))
         (max-y (vy max-point))
         (min-z (vz min-point))
         (max-z (vz max-point)))

    (cond (min-point
           (kdtree:each-point points
                              (lambda (pt)
                                (setf min-x (min (vx pt) min-x))
                                (setf max-x (max (vx pt) max-x))

                                (setf min-y (min (vy pt) min-y))
                                (setf max-y (max (vy pt) max-y))

                                (setf min-z (min (vz pt) min-z))
                                (setf max-z (max (vz pt) max-z))))
           )
          (t
           (values (vec3 -1 -1 -1) (vec3 1 1 1))))))

(defun kdtree-view (kdt &optional (color (vec4 0 1 0 1)))
  (let* ((pts (kdtree:to-list kdt))
         (pt-array (make-array (length pts) :initial-contents pts)))
    (from-points pt-array color)))

(defmethod bounding-box ((points simple-vector))
  (loop for pt across points
     minimizing (vx pt) into min-x
     maximizing (vx pt) into max-x
     minimizing (vy pt) into min-y
     maximizing (vy pt) into max-y
     minimizing (vz pt) into min-z
     maximizing (vz pt) into max-z
     finally (return (values (vec3 min-x min-y min-z) (vec3 max-x max-y max-z)))))

(defun from-points (all-points &optional (color (vec4 0 1 0 1)))
  (let ((prims (make-instance 'primitives)))
    (loop for pt across all-points
       do
         (add-point prims pt color))
    prims))

(defun from-point-list (all-points &optional (color (vec4 0 1 0 1)))
  (let ((prims (make-instance 'primitives)))
    (multiple-value-bind (min-pt max-pt) (bounding-box all-points)
      (loop for pt in all-points
         for mapped-pt = (map-pt pt min-pt max-pt) then (map-pt pt min-pt max-pt)
         do
           (add-point prims mapped-pt color)))
    prims))

;; (defun show-kdtree-inner (prims kdt xmin ymin zmin xmax ymax zmax &optional (depth 0))
;;   (let ((left (kd-tree-node-left kdt))
;;         (right (kd-tree-node-right kdt))
;;         (pt (kd-tree-node-pt kdt))
;;         (split-direction (mod depth 3)))
;;     (cond ((= 0 split-direction)
;;            (add-line prims (vec3 (vx pt) ymin zmin xmin (vy pt)
;;            (add-line-coords svg  xmin (vx pt) )
;;            (when left
;;              (show-kdtree-inner prims left (- radius ymin (point-x pt) ymax (+ 1 depth)))
;;           (if right
;;               (inner-svg svg right (point-x pt) ymin xmax ymax (+ 1 depth))))
;;         (progn
;;           (svg:add-line-coords svg xmin (point-y pt) xmax (point-y pt))
;;           (if left
;;               (inner-svg svg left xmin ymin xmax (point-y pt) (+ 1 depth)))
;;           (if right
;;               (inner-svg svg right xmin (point-y pt) xmax ymax (+ 1 depth)))))
    
;;     (svg:add-point-coords svg (point-x pt) (point-y pt))))

;; (defun show-kdtree (kdt min-value max-value fname)
;;   (let ((svg (svg:make-svg))
;;         (transform (mtranslation (v- (vec3 0 0 0) min-value))))
;;     (inner-svg svg kdt transform 0 0 800 800)
;;     (svg:to-file-name svg fname)))

(defun 2d-distance-squared (p1 p2)
  (let* ((xd (- (vx p1) (vx p2)))
         (yd (- (vy p1) (vy p2)))
         (rval (+ (* xd xd) (* yd yd))))
    rval))

(defun kdtree-reduce-points (pts threshold)
  (let ((kdt (kdtree:create-kd-tree)))
    (loop for pt across pts do
         (let ((nearest (kdtree:nearest kdt pt :distance-function #'2d-distance-squared)))
           (when (or (null nearest) (> (vlength (v- pt nearest)) threshold))
             (kdtree:add-point kdt pt))))
    (format t "Size of kdtree is ~a~%" (kdtree:pt-count kdt))
    (clgl:kdtree-view kdt (vec4 0.0 1.0 0.0 0.5))))


(defun to-rectangular (delta)
  "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
  (vec3
   (* (vz delta) (cos (vy delta)) (cos (vx delta)))
   (* (vz delta) (sin (vy delta)) (cos (vx delta)))
   (* (vz delta) (sin (vx delta)))))

(defun fractal-tree (&key (maxdepth 4) (theta-limbs 4) (phi-limbs 4) (color (vec4 0 1 0 1)))
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((prims (make-instance 'primitives)))
    (labels
        ((draw-tree (current delta depth)
           ;;(add-point prims current color)
           (add-line prims current (v+ current (to-rectangular delta)) color)
           (when (> depth 0)
             (let ((next-base (v+ current (to-rectangular delta)))
                   (d-phi (/ (* 2 pi) phi-limbs))
                   (d-theta (/ pi phi-limbs)))
               (dotimes (i theta-limbs)
                 (dotimes (j phi-limbs)
                   (draw-tree
                    next-base
                    (vec3 (+ (/ (vx delta) 3.0) (random 0.0125))
                          (+ (* i d-phi) (random 0.75))
                          (+ (* j d-theta) (random 0.75)))
                    (- depth 1))))))))
      (draw-tree (vec3 0 0 0) (vec3 0.5 pi 0) maxdepth))
    prims))

(defstruct attractor-variables
  (a 2.24)
  (b 0.43)
  (c -0.65)
  (d -2.43)
  (e 1.0))

(defun random-range (min max)
  (+ min (random (- max min))))


(defun random-attractor (&key
                           (a-max 2.0) (a-min -2.0)
                           (b-max 2.0) (b-min -2.0)
                           (c-max 2.0) (c-min -2.0)
                           (d-max 2.0) (d-min -2.0)
                           (e-max 2.0) (e-min -2.0))
  (make-attractor-variables
   :a (random-range a-min a-max)
   :b (random-range b-min b-max)
   :c (random-range c-min c-max)
   :d (random-range d-min d-max)
   :e (random-range e-min e-max)))

(defun strange-attractor (&key
                            (min-value (vec3 -2.0 -2.0 -2.0))
                            (max-value (vec3 2.0 2.0 2.0))
                            (iterations 500000)
                            (at-vars (make-attractor-variables)))
  "Draw a strange-attractor fractal into file-name, zoomed into the window specified by xxmin,xxmax and yymin,yymax.  iterations is the number of iterations to run.  a, b, c, d, and e are the parameters of the strange attractor and can be modified for various effects."
  (let ((prims (make-instance 'primitives))
        (x 0)
        (y 0)
        (z 0)
        (a (attractor-variables-a at-vars))
        (b (attractor-variables-b at-vars))
        (c (attractor-variables-c at-vars))
        (d (attractor-variables-d at-vars))
        (e (attractor-variables-e at-vars)))
    (dotimes (i iterations)
      (let ((xx (- (sin (* a y)) (* z (cos (* b x)))))
            (yy (- (sin (* c x)) (cos (* d y))))
            (zz (* e (sin x))))
        (add-point prims (map-pt (vec3 x y z) min-value max-value) (vec4 xx yy zz 1.0))
        (setf x xx
              y yy
              z zz)))
    prims))

(defun affine-transform (&key
                           (iterations 500000)
                           (xforms (list (cons 0.01 (mat4 '(0.0 0.0 0.0 0.0
                                                          0.0 0.16 0.0 0.0
                                                          0.0 0.0 1.0 0.0
                                                          0.0 0.0 0.0 1.0)))
                                         (cons 0.85 (mat4 '(0.85 0.04 0.0 0.0
                                                          -0.04 0.85 0.0 1.60
                                                          0.0 0.0 1.0 0.0
                                                          0.0 0.0 0.0 1.0)))
                                         (cons 0.07 (mat4 '(0.20 -0.26 0.0 0.0
                                                          0.23 0.22 0.0 1.60
                                                          0.0 0.0 1.0 0.0
                                                          0.0 0.0 0.0 1.0)))
                                         (cons 0.07 (mat4 '(-0.15 0.28 0.0 0.0
                                                          0.26 0.24 0.0 0.44
                                                          0.0 0.0 1.0 0.0
                                                          0.0 0.0 0.0 1.0))))))
  "Draw a strange-attractor fractal into file-name, zoomed into the window specified by xxmin,xxmax and yymin,yymax.  iterations is the number of iterations to run.  a, b, c, d, and e are the parameters of the strange attractor and can be modified for various effects."
  (let ((prims (make-instance 'primitives))
        (the-point (vec4 0 0 0 1.0)))
    (loop
       for i below iterations
       do
         (loop
            for item in xforms
            for choice from 0
            for cprob = (random 1.0) then (- cprob (car item))
            when (< cprob (car item))
            do
              (setf the-point (m* (cdr item) the-point))
              (format t "chose ~a because ~a~%" item cprob )
              (add-point prims (vxyz the-point) (vec4 0.0 1.0 0.0 1.0))))
    prims))

(defun file-visualizer (file-name)
  (let ((prims (make-instance 'primitives))
        (file-buffer (read-file-into-byte-vector file-name)))
    (loop for i below (1- (length file-buffer)) by 3
       do
         (add-point prims (vec3 (/ (aref file-buffer i) 256.0)
                                (/ (aref file-buffer (+ i 1)) 256.0)
                                (/ (aref file-buffer (+ i 2)) 256.0))
                    (vec4 0.1 0.5 0.2 0.5)))
    prims))
