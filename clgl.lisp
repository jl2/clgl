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
  (let ((viewer (make-instance 'viewer :viewport (make-instance 'clgl:simple-viewport :distance 4))))
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
             (vec3 (funcall xf uv nv)
                   (funcall yf uv nv)
                   (funcall zf uv nv))

             (vec3 (funcall xf nu vv)
                   (funcall yf nu vv)
                   (funcall zf nu vv))
             (vec3 (funcall xf uv vv)
                   (funcall yf uv vv)
                   (funcall zf uv vv))

             color)

    (funcall tri-function obj
             (vec3 (funcall xf nu vv)
                   (funcall yf nu vv)
                   (funcall zf nu vv))

             (vec3 (funcall xf uv nv)
                   (funcall yf uv nv)
                   (funcall zf uv nv))

             (vec3 (funcall xf nu nv)
                   (funcall yf nu nv)
                   (funcall zf nu nv))

             color)))

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
        (dv (/ (- v-max v-min) v-steps)))
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

(defun bounding-box (points)
  (loop for pt in points
     minimizing (gpxtools:gpx-pt-lat pt) into min-lat
     maximizing (gpxtools:gpx-pt-lat pt) into max-lat
     minimizing (gpxtools:gpx-pt-lon pt) into min-lon
     maximizing (gpxtools:gpx-pt-lon pt) into max-lon
     minimizing (gpxtools:gpx-pt-ele pt) into min-ele
     maximizing (gpxtools:gpx-pt-ele pt) into max-ele
     finally (return (values min-lat max-lat min-lon max-lon min-ele max-ele))))

(defun map-value (val min-val max-val)
  (- (* 2 (/ (- val min-val) (- max-val min-val))) 1.0))

(defun map-pt (lat lon ele min-lat max-lat min-lon max-lon min-ele max-ele)
  (let ((lat (map-value lat min-lat max-lat))
        (lon (map-value lon min-lon max-lon))
        (ele (map-value ele min-ele max-ele)))
    (values lat lon ele)))

(defun gpx-heatmap (directory)
  (let* ((gpx-pts (make-instance 'primitives))
         (files (directory (format nil "~a/*.gpx" directory)))
         (all-points (apply (curry #'concatenate 'list) (mapcar (compose #'gpxtools:collect-points #'gpxtools:read-gpx) files))))
    (multiple-value-bind (min-lat max-lat min-lon max-lon min-ele max-ele) (bounding-box all-points)
      (dolist (gpt all-points)
        (multiple-value-bind (y-value x-value z-value) (map-pt (gpxtools:gpx-pt-lat gpt)
                                                               (gpxtools:gpx-pt-lon gpt)
                                                               (gpxtools:gpx-pt-ele gpt)
                                                               min-lat max-lat
                                                               min-lon max-lon
                                                               min-ele max-ele)
          (add-point gpx-pts (vec3
                              x-value
                              y-value
                              z-value)
                     (vec4 0 1 0 0.25)))))
    gpx-pts))
