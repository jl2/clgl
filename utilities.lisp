;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(deftype point ()
  '(or vec3))

(deftype normal ()
  '(or vec3))

(deftype color ()
  '(or vec4))

(defun red (color)
  (vx color))

(defun green (color)
  (vy color))

(defun blue (color)
  (vz color))

(defun alpha (color)
  (vw color))

(defun read-file (fname)
  (with-open-file (ins fname)
    (let ((seq (make-array (file-length ins)
                           :element-type (stream-element-type ins))))
      (read-sequence seq ins) seq)))

(defun randr (&key (min -1.0f0) (max 1.0f0))
  (+ min (random (- max min))))

(defun random-color ()
  (vec4 (randr :min 0.25 :max 1.0)
        (randr :min 0.25 :max 1.0)
        (randr :min 0.25 :max 1.0)
        1.0f0))

