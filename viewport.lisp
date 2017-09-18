;;;; viewport.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:clgl)

(defclass viewport ()
  ((projection :initarg :projection :initform (make-instance 'orthographic))))

(defgeneric get-transform-matrix (view))
(defgeneric get-projection-matrix (view))

(defmethod get-projection-matrix (view)
  (get-transform-matrix (slot-value view 'projection)))

(defclass 2d-viewport (viewport)
  ((projection :initarg :projection :initform (make-instance 'orthographic))
   (radius :initarg :radius :initform 10.0)))

(defmethod get-transform-matrix ((view 2d-viewport))
  (declare (ignorable view))
  (with-slots (radius projection) view
    (setf projection (make-instance 'orthographic :left (- radius)
                                    :left (- radius)
                                    :right radius
                                    :top radius
                                    :bottom (- radius)
                                    :near (- radius)
                                    :far radius))))


(defclass 3d-viewport (viewport)
  ((projection :initarg :projection :initform (make-instance 'perspective))))

(defclass spherical-viewport (3d-viewport)
  ((radius :initarg :radius :initform 10.0f0)
   (theta :initarg :theta :initform (/ pi 4))
   (phi :initarg :phi :initform (/ pi 4))))

(defmethod get-transform-matrix ((view spherical-viewport))
  (with-slots (radius theta phi) view
    (let ((return-value (m* 
                         (mrotation +vy+ theta)
                         (mrotation +vx+ phi)
                         (mtranslation (vec3 0 0 (- radius))))))
      (declare (type mat4 return-value))
      (setf (mcref return-value 3 3) (/ 1.0 radius))
      return-value)))

(defclass look-at-viewport (3d-viewport)
  ((eye :initarg :eye :initform (vec3 16.0f0 16.0f0 16.0f0))
   (center :initarg :center :initform (vec3 0.0f0 0.0f0 0.0f0))
   (up :initarg :up :initform +vz+)))


(defmethod get-transform-matrix ((view look-at-viewport))
  (with-slots (eye center up) view
    (let ((ilen (/ 1.0 (vlength (v- center eye))))
          (return-value (mlookat eye center up)))
       ;; (setf (mcref return-value 3 3) ilen)
       ;; return-value)))
      (m* (mscaling (vec3 ilen ilen ilen)) return-value))))
