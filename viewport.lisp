;;;; viewport.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defgeneric get-transform (view))

(defclass projection ()
  ())

(defclass orthographic (projection)
  ((left :initarg :left  :initform -1.0)
   (right :initarg :right :initform 1.0)
   (top :initarg :top :initform 1.0)
   (bottom :initarg :bottom :initform -1.0)
   (near :initarg :near :initform -1.0)
   (far :initarg :far :initform 1.0)))

(defmethod get-transform ((view orthographic))
  (with-slots (left right top bottom near far) view
    (mortho left right bottom top near far)))

(defclass perspective (projection)
  ((fovy :initarg :fovy :initform (* 30 (/ pi 180)))
   (aspect :initarg :aspect :initform 1.2)
   (near :initarg :near :initform 10.0)
   (far :initarg :far :initform 2.0)))

(defmethod get-transform ((view perspective))
  (with-slots (fovy aspect near far) view
    ;; (let* ((mat (mperspective fovy aspect near far))
    ;;        (tmp (aref (marr4 mat) 11)))
    ;;   (setf (aref (marr4 mat) 11) (aref (marr4 mat) 14))
    ;;   (setf (aref (marr4 mat) 14) tmp)
    ;;   mat)
    (mperspective fovy aspect near far)
    ))

(defclass viewport ()
  ((projection :initarg :projection :initform (make-instance 'orthographic))))

(defclass 2d-viewport (viewport)
  ((radius :initarg :radius :initform 10.0)))

(defmethod get-transform ((view 2d-viewport))
  (declare (ignorable view))
  (with-slots (radius) view
    (with-slots (left right top bottom near far) (slot-value view 'projection)
      (setf left (- radius))
      (setf right radius)
      (setf top radius)
      (setf bottom (- radius))
      (setf near (- radius))
      (setf far radius))))

(defclass 3d-viewport (viewport)
  ((projection :initarg :projection :initform (make-instance 'perspective))))

(defclass spherical-viewport (3d-viewport)
  ((radius :initarg :radius :initform 10.0f0)
   (theta :initarg :theta :initform (/ pi 4))
   (phi :initarg :phi :initform (/ pi 4))))

(defmethod get-transform ((view spherical-viewport))
  (with-slots (radius theta phi) view
    (m* 
     (mrotation +vy+ theta)
     (mrotation +vx+ phi)
     (mtranslation (vec3 0 0  radius)))))

(defclass look-at-viewport (3d-viewport)
  ((eye :initarg :eye :initform (vec3 16.0f0 6.0f0 6.0f0))
   (center :initarg :center :initform (vec3 0.0f0 0.0f0 0.0f0))
   (up :initarg :up :initform +vz+)))

(defmethod get-transform ((view look-at-viewport))
  (with-slots (eye center up) view
    (let ((vlen  (vlength (v- eye center)))
          (mat (mlookat eye center up)))
      (m* mat 
          (mscaling (vec3 (/ 1.0f0 vlen) (/ 1.0f0 vlen) (/ 1.0f0 vlen)))))))


(defun apply-view-transformation (viewport object-transform)
  (with-slots (projection) viewport
    (m* 
     (get-transform projection)
     (get-transform viewport)
     
     object-transform
        
        )))
