;;; -*- lisp -*-
(defpackage turtar/thing
  (:use :cl :oliphaunt
        :turtar/entity)
  (:export #:thing))
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



(defstruct vec3 
  (x :x :type real)
  (y :y :type real)
  (z :z :type real))

(defun vec3 (x y z)
  (check-type x real)
  (check-type y real)
  (check-type z real)
  (make-vec3 :x x :y y :z z))

(let ((a (vec3 1 2/3 .25)))
  (assert (= (vec3-x a) 1))
  (assert (= (vec3-y a) 2/3))
  (assert (= (vec3-z a) .25)))

(defun vec3= (a b &rest more)
  (and (and (= (vec3-x a) (vec3-x b))
            (= (vec3-y a) (vec3-y b))
            (= (vec3-z a) (vec3-z b)))
       (if more 
           (apply #'vec3= a more)
           t)))

(let* ((a (vec3 1 2 3))
       (b (copy-vec3 a)))
  (assert (vec3= a b))
  (assert (not (eql a b))))

(defun vec3+ (a b &rest more)
  (if more
      (reduce #'vec3+ (append (list a b) more))
      (vec3 (+ (vec3-x a) (vec3-x b))
            (+ (vec3-y a) (vec3-y b))
            (+ (vec3-z a) (vec3-z b)))))

(assert (vec3= (vec3 2 3 4) (vec3+ (vec3 1 1 1) (vec3 1 2 3))))
(assert (vec3= (vec3 3 4 5) (vec3+ (vec3 1 1 1) (vec3 1 1 1) (vec3 1 2 3))))

(defgeneric vec3* (a b &rest more))

(defmethod vec3* :around (a b &rest more)
  (if more
      (reduce #'vec3* (append (list a b) more))
      (call-next-method)))

(defmethod vec3* ((a vec3) (b real) &rest more)
  (assert (null more))
  (vec3 (* (vec3-x a) b)
        (* (vec3-y a) b)
        (* (vec3-z a) b)))

(defmethod vec3* ((a real) (b vec3) &rest more)
  (assert (null more))
  (vec3 (* a (vec3-x b))
        (* a (vec3-y b))
        (* a (vec3-z b))))

(assert (vec3= (vec3 2 4 6) (vec3* 2 (vec3 1 2 3))))
(assert (vec3= (vec3 2 4 6) (vec3* (vec3 1 2 3) 2)))
(assert (vec3= (vec3 4 8 12) (vec3* 2 (vec3 1 2 3) 2)))

(defun vec3-zerop (vec)
  (and (zerop (vec3-x vec))
       (zerop (vec3-y vec))
       (zerop (vec3-z vec))))

(assert (vec3-zerop (vec3 0 0 0)))
(assert (not (vec3-zerop (vec3 1 0 0))))
(assert (not (vec3-zerop (vec3 7 8 9))))



(defstruct rot2
  (θ :θ :type real)
  (φ :φ :type real))

(defun rot2 (θ φ)
  (let ((θ₀ (if (< (- pi) θ pi)
                θ
                (- (mod (+ θ pi) (* 2 pi)) pi)))
        (φ₀ (if (< (/ pi -2) φ (/ pi 2))
                φ
                (- (mod (+ φ (/ pi 2)) pi) (/ pi 2)))))
    (make-rot2 :θ θ₀ :φ φ₀)))

(defun rot2-theta (rot) (rot2-θ rot))
(defun rot2-phi (rot) (rot2-φ rot))

(assert (= 1 (rot2-θ (rot2 1 1/2))))
(assert (= 1/2 (rot2-φ (rot2 1 1/2))))
(assert (= .5 (rot2-φ (rot2 1 (+ pi pi .5)))))

(defun rot2= (a b &rest more)
  (and (and (= (rot2-θ a) (rot2-θ b))
            (= (rot2-φ a) (rot2-φ b)))
       (if more
           (apply #'rot2= a more)
           t)))

(assert (rot2= (rot2 1 1) (rot2 1 1)))

(defun rot2+ (a b &rest more)
  (if more
      (reduce #'rot2+ (cons (list a b) more))
      (rot2 (+ (rot2-θ a) (rot2-θ b))
            (+ (rot2-φ a) (rot2-φ b)))))

(assert (rot2= (rot2+ (rot2 0 0) (rot2 1 2)) (rot2 1 2)))
(assert (rot2= (rot2+ (rot2 0 pi) (rot2 1 pi)) (rot2 1 0)))

(defgeneric rot2* (a b &rest more))

(defmethod rot2* :around (a b &rest more)
  (if more
      (reduce #'rot2* (cons (list a b) more))
      (call-next-method)))

(defmethod rot2* ((a rot2) (b real) &rest more)
  (assert (null more))
  (rot2 (* (rot2-θ a) b)
        (* (rot2-φ a) b)))

(defmethod rot2* ((a real) (b rot2) &rest more)
  (assert (null more))
  (rot2 (* a (rot2-θ b))
        (* a (rot2-φ b))))

(assert (rot2= (rot2 2 0) (rot2* 2 (rot2 1 0))))
(assert (rot2= (rot2 2 0) (rot2* (rot2 1 0) 2)))

(defun rot2-zerop (rot)
  (and (zerop (rot2-θ rot))
       (zerop (rot2-φ rot))))

(assert (rot2-zerop (rot2 0 0)))
(assert (not (rot2-zerop (rot2 45 72))))



(defclass body () ())

(defgeneric body-equal (a b) (:method ((a body) (b body)) nil))

(defclass spherical-body (body)
  ((radius :initarg :radius :initform 1 :reader spherical-body-radius)))

(defmethod body-equal ((a spherical-body) (b spherical-body))
  (= (spherical-body-radius a) (spherical-body-radius b)))

(defclass rectangular-body (body)
  ((length :type real :initarg :length :initform 1 :reader rectangular-body-length)
   (width :type real :initarg :width :initform 1 :reader rectangular-body-width)
   (depth :type real :initarg :depth :initform 1 :reader rectangular-body-depth)))

(defmethod body-equal ((a rectangular-body) (b rectangular-body))
  (and (= (rectangular-body-length a) (rectangular-body-length b))
       (= (rectangular-body-width a) (rectangular-body-width b))
       (= (rectangular-body-depth a) (rectangular-body-depth b))))



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
   (rotation :type rot2 :initarg :rotation :initform (rot2 0 0) :reader thing-rotation)
   (angular-velocity :type rot2 :initarg :angular-velocity :initform (rot2 0 0) :reader thing-angular-velocity)
   (angular-acceleration :type rot2 :initarg :angular-acceleration :initform (rot2 0 0)
                         :reader thing-angular-acceleration)
   (angular-jerk :type rot2 :initarg :angular-jerk :initform (rot2 0 0) :reader thing-angular-jerk)
   (angular-snap :type rot2 :initarg :angular-snap :initform (rot2 0 0) :reader thing-angular-snap)
   (angular-crackle :type rot2 :initarg :angular-crackle :initform (rot2 0 0) :reader thing-angular-crackle)
   (angular-pop :type rot2 :initarg :angular-pop :initform (rot2 0 0) :reader thing-angular-pop)
   (angular-lock :type rot2 :initarg :angular-lock :initform (rot2 0 0) :reader thing-angular-lock)
   (angular-drop :type rot2 :initarg :angular-drop :initform (rot2 0 0) :reader thing-angular-drop)
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
       (every #'identity (mapcar #'rot2= 
                                 (coerce (thing-rotation-and-derivatives a) 'list)
                                 (coerce (thing-rotation-and-derivatives b) 'list)))
       (= (thing-position+rotation-updated a) (thing-position+rotation-updated b))
       (= (thing-mass a) (thing-mass b))
       (body-equal (thing-body a) (thing-body b))))

(let* ((thingus (make-instance 'thing))
       (thingie (copy-object thingus)))
  (assert (component-equal thingus thingie))
  (assert (> (component-version thingie) (component-version thingus))))



(defun thing-update-position+rotation-derivatives (&key position rotation Δτ)
  (let ((position-derivatives (copy-array position))
        (rotation-derivatives (copy-array rotation)))
    (loop for derivative from 7 downto 0
       do (setf (aref position-derivatives derivative)
                (vec3+ (aref position-derivatives derivative) 
                       (vec3* Δτ (aref position-derivatives (1+ derivative)))))
       do (setf (aref rotation-derivatives derivative)
                (rot2+ (aref rotation-derivatives derivative) 
                       (rot2* Δτ (aref rotation-derivatives (1+ derivative))))))
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

(defun thing-update-position+rotation (thing &key target-real-time Δτ delta-t)
  (assert (or (null target-real-time) (null Δτ) delta-t))
  (assert (or (null target-real-time) Δτ (null delta-t)))
  (assert (or target-real-time (null Δτ) (null delta-t)))
  (check-type target-real-time (or null (real 0 *)))
  (check-type Δτ (or null (real 0 *)))
  (check-type delta-t (or null (real 0 *)))
  
  (multiple-value-bind (position-derivatives rotation-derivatives)
      (thing-update-position+rotation-derivatives :position (thing-position-and-derivatives thing)
                                                  :rotation (thing-rotation-and-derivatives thing)
                                                  :Δτ (if target-real-time
                                                          (- target-real-time
                                                             (thing-position+rotation-updated thing))
                                                          (or Δτ delta-t)))
    (mutate-position+rotation-derivatives thing position-derivatives rotation-derivatives)))

(let* ((before (make-instance 'thing :position (vec3 5 8 2)))
       (after (thing-update-position+rotation before :Δτ 1)))
  (assert (vec3= (thing-position after) (vec3 5 8 2))))

(let* ((before (make-instance 'thing :position (vec3 4 5 6) :velocity (vec3 10 0 0)))
       (after (thing-update-position+rotation before :Δτ 1)))
  (assert (vec3= (thing-position after) (vec3 14 5 6))))

(let* ((before (make-instance 'thing :position (vec3 4 5 6) :snap (vec3 10 0 0)))
       (after (thing-update-position+rotation before :Δτ 1)))
  (assert (vec3= (thing-position after) (vec3 14 5 6))))


