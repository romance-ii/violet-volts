;;; -*- lisp -*-
(defpackage turtar/thing
  (:use :cl :oliphaunt :turtar/entity :turtar/geometry)
  (:export #:thing
           #:thing-update-position+rotation
           #:thing-resting-p))
(in-package :turtar/thing)

;;; Thing
;;;
;;; Part of Turtar
;;;
;;; Copyright © 2016, Bruce-Robert Fenn Pocock
;;;
;;; This program is free  software: you can redistribute it and/or  modify it under the terms of  the GNU Affero General
;;; Public License as  published by the Free Software Foundation,  either version 3 of the License,  or (at your option)
;;; any later version.
;;;
;;; This program is distributed in  the hope that it will be useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of  MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU  Affero General Public  License for
;;; more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public License  along with  this program. If  not, see
;;; <http://www.gnu.org/licenses/>.



(defclass thing (component)
  ((universe :initarg :universe :initform nil :reader thing-universe)
   ;;; — Position and its derivatives
   (position :type vec3 :initarg :position :initform (vec3 0 0 0) :reader thing-position)
   (velocity :type vec3 :initarg :velocity :initform (vec3 0 0 0) :reader thing-velocity)
   (acceleration :type vec3 :initarg :acceleration :initform (vec3 0 0 0) :reader thing-acceleration)
   (jerk :type vec3 :initarg :jerk :initform (vec3 0 0 0) :reader thing-jerk)
   (snap :type vec3 :initarg :snap :initform (vec3 0 0 0) :reader thing-snap)
   (crackle :type vec3 :initarg :crackle :initform (vec3 0 0 0) :reader thing-crackle)
   (pop :type vec3 :initarg :pop :initform (vec3 0 0 0) :reader thing-pop)
   (lock :type vec3 :initarg :lock :initform (vec3 0 0 0) :reader thing-lock)
   (drop :type vec3 :initarg :drop :initform (vec3 0 0 0) :reader thing-drop)
   ;;; — Rotation and its derivatives
   (rotation :type rot3 :initarg :rotation :initform (rot3 0 0 0) :reader thing-rotation)
   (angular-velocity :type rot3 :initarg :angular-velocity :initform (rot3 0 0 0) :reader thing-angular-velocity)
   (angular-acceleration :type rot3 :initarg :angular-acceleration :initform (rot3 0 0 0)
                         :reader thing-angular-acceleration)
   (angular-jerk :type rot3 :initarg :angular-jerk :initform (rot3 0 0 0) :reader thing-angular-jerk)
   (angular-snap :type rot3 :initarg :angular-snap :initform (rot3 0 0 0) :reader thing-angular-snap)
   (angular-crackle :type rot3 :initarg :angular-crackle :initform (rot3 0 0 0) :reader thing-angular-crackle)
   (angular-pop :type rot3 :initarg :angular-pop :initform (rot3 0 0 0) :reader thing-angular-pop)
   (angular-lock :type rot3 :initarg :angular-lock :initform (rot3 0 0 0) :reader thing-angular-lock)
   (angular-drop :type rot3 :initarg :angular-drop :initform (rot3 0 0 0) :reader thing-angular-drop)
   ;;; — Simple self-update of motion without interaction
   (position+rotation-updated :type real :initform (get-internal-real-time) 
                              :reader thing-position+rotation-updated)
   ;;; — Mass and shape
   (mass :type real :initarg :mass :initform 1 :reader thing-mass)
   (body :type body :initarg :body :initform (make-instance 'spherical-body :radius 1) :reader thing-body)))



(defun thing-jounce (thing) (thing-snap thing))
(defun thing-angular-jounce (thing) (thing-angular-snap thing))

(defgeneric thing-position-derivative (thing index)
  (:method ((thing thing) (index (eql 0))) (thing-position thing))
  (:method ((thing thing) (index (eql 1))) (thing-velocity thing))
  (:method ((thing thing) (index (eql 2))) (thing-acceleration thing))
  (:method ((thing thing) (index (eql 3))) (thing-jerk thing))
  (:method ((thing thing) (index (eql 4))) (thing-snap thing))
  (:method ((thing thing) (index (eql 5))) (thing-crackle thing))
  (:method ((thing thing) (index (eql 6))) (thing-pop thing))
  (:method ((thing thing) (index (eql 7))) (thing-lock thing))
  (:method ((thing thing) (index (eql 8))) (thing-drop thing)))

(defun thing-position-and-derivatives (thing)
  (check-type thing thing)
  (map 'vector (curry #'thing-position-derivative thing)
       '(0 1 2 3 4 5 6 7 8)))

(defgeneric thing-rotation-derivative (thing index)
  (:method ((thing thing) (index (eql 0))) (thing-rotation thing))
  (:method ((thing thing) (index (eql 1))) (thing-angular-velocity thing))
  (:method ((thing thing) (index (eql 2))) (thing-angular-acceleration thing))
  (:method ((thing thing) (index (eql 3))) (thing-angular-jerk thing))
  (:method ((thing thing) (index (eql 4))) (thing-angular-snap thing))
  (:method ((thing thing) (index (eql 5))) (thing-angular-crackle thing))
  (:method ((thing thing) (index (eql 6))) (thing-angular-pop thing))
  (:method ((thing thing) (index (eql 7))) (thing-angular-lock thing))
  (:method ((thing thing) (index (eql 8))) (thing-angular-drop thing)))

(defun thing-rotation-and-derivatives (thing)
  (check-type thing thing)
  (map 'vector (curry #'thing-rotation-derivative thing)
       '(0 1 2 3 4 5 6 7 8)))

(defmethod component-equal ((a thing) (b thing))
  (and (equalp (thing-universe a) (thing-universe b))
       (every #'identity (mapcar #'vec3= 
                                 (coerce (thing-position-and-derivatives a) 'list)
                                 (coerce (thing-position-and-derivatives b) 'list)))
       (every #'identity (mapcar #'rot3= 
                                 (coerce (thing-rotation-and-derivatives a) 'list)
                                 (coerce (thing-rotation-and-derivatives b) 'list)))
       (= (thing-position+rotation-updated a) (thing-position+rotation-updated b))
       (= (thing-mass a) (thing-mass b))
       (body-equal (thing-body a) (thing-body b))))

(let* ((thingus (make-instance 'thing))
       (thingie (copy-object thingus)))
  (assert (component-equal thingus thingie))
  (assert (> (component-version thingie) (component-version thingus))))

(defmethod thing-resting-p (thing)
  (and (every #'vec3-zerop (subseq (thing-position-and-derivatives thing) 1))
       (every #'rot3-zerop (subseq (thing-rotation-and-derivatives thing) 1))))



(defun thing-update-position+rotation-derivatives (&key position rotation Δt)
  (let ((position-derivatives (copy-array position))
        (rotation-derivatives (copy-array rotation)))
    (loop for derivative from 7 downto 0
       do (setf (aref position-derivatives derivative)
                (vec3+ (aref position-derivatives derivative) 
                       (vec3* Δt (aref position-derivatives (1+ derivative)))))
       do (setf (aref rotation-derivatives derivative)
                (rot3+ (aref rotation-derivatives derivative) 
                       (rot3* Δt (aref rotation-derivatives (1+ derivative))))))
    (values position-derivatives rotation-derivatives)))

(defun mutate-position+rotation-derivatives (thing position-derivatives rotation-derivatives)
  (mutate thing
          :position (aref position-derivatives 0)
          :velocity (aref position-derivatives 1)
          :acceleration (aref position-derivatives 2)
          :jerk (aref position-derivatives 3)
          :snap (aref position-derivatives 4)
          :crackle (aref position-derivatives 5)
          :pop (aref position-derivatives 6)
          :lock (aref position-derivatives 7)
          :drop (aref position-derivatives 8)
          
          :rotation (aref rotation-derivatives 0)
          :angular-velocity (aref rotation-derivatives 1)
          :angular-acceleration (aref rotation-derivatives 2)
          :angular-jerk (aref rotation-derivatives 3)
          :angular-snap (aref rotation-derivatives 4)
          :angular-crackle (aref rotation-derivatives 5)
          :angular-pop (aref rotation-derivatives 6)
          :angular-lock (aref rotation-derivatives 7)
          :angular-drop (aref rotation-derivatives 8)))

(defun thing-update-position+rotation (thing &key target-real-time Δt delta-t)
  (assert (or (null target-real-time) (null Δt) delta-t))
  (assert (or (null target-real-time) Δt (null delta-t)))
  (assert (or target-real-time (null Δt) (null delta-t)))
  (check-type target-real-time (or null (real 0 *)))
  (check-type Δt (or null (real 0 *)))
  (check-type delta-t (or null (real 0 *)))
  
  (multiple-value-bind (position-derivatives rotation-derivatives)
      (thing-update-position+rotation-derivatives :position (thing-position-and-derivatives thing)
                                                  :rotation (thing-rotation-and-derivatives thing)
                                                  :Δt (if target-real-time
                                                          (- target-real-time
                                                             (thing-position+rotation-updated thing))
                                                          (or Δt delta-t)))
    (mutate-position+rotation-derivatives thing position-derivatives rotation-derivatives)))

(let* ((before (make-instance 'thing :position (vec3 5 8 2)))
       (after (thing-update-position+rotation before :Δt 1)))
  (assert (vec3= (thing-position after) (vec3 5 8 2))))

(let* ((before (make-instance 'thing :position (vec3 4 5 6) :velocity (vec3 10 0 0)))
       (after (thing-update-position+rotation before :Δt 1)))
  (assert (vec3= (thing-position after) (vec3 14 5 6))))

(let* ((before (make-instance 'thing :position (vec3 4 5 6) :snap (vec3 10 0 0)))
       (after (thing-update-position+rotation before :Δt 1)))
  (assert (vec3= (thing-position after) (vec3 14 5 6))))

(defmethod distance-between ((a thing) (b thing))
  (distance-between (thing-position a) (thing-position b)))

(assert (= 1 (distance-between (vec3 0 0 0) (vec3 1 0 0))))
(assert (= 0 (distance-between (vec3 4 5 6) (vec3 4 5 6))))
(assert (= (sqrt 14) (distance-between (vec3 0 0 0) (vec3 1 2 3))))
