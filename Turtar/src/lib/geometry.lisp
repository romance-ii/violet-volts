;;; -*- lisp -*-
(defpackage turtar/geometry
  (:use :cl :oliphaunt)
  (:export #:vec3 
           #:vec3+
           #:vec3-
           #:vec3=
           #:vec3-zerop
           #:vec3-vector
           #:rot3
           #:rot3+
           #:rot3-
           #:rot3=
           #:rot3-zerop
           #:rot3-matrix
           #:rot3-quaternion
           #:rot3°
           #:rot3-degrees
           #:rad-deg
           #:deg-rad
           #:distance-between
           #:body
           #:body-intersects-p
           #:body-equal
           #:body-bounding-box
           #:body-bounding-sphere))
(in-package :turtar/geometry)

;;; Geometry
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

(defmethod vec3+ ((a vec3) (b vec3))
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-y b))
        (+ (vec3-z a) (vec3-z b))))

(assert (vec3= (vec3 4 6 8) (vec3+ (vec3 1 1 1) (vec3 3 5 7))))

(defmethod vec3- ((a vec3) (b vec3))
  (vec3 (- (vec3-x a) (vec3-x b))
        (- (vec3-y a) (vec3-y b))
        (- (vec3-z a) (vec3-z b))))

(assert (vec3= (vec3 4 6 8) (vec3- (vec3 5 7 9) (vec3 1 1 1))))

(defmethod vec3+ ((a vec3) (b real))
  (vec3 (+ (vec3-x a) b)
        (+ (vec3-y a) b)
        (+ (vec3-z a) b)))

(assert (vec3= (vec3 4 6 8) (vec3+ (vec3 3 5 7) 1)))

(defmethod vec3- ((a vec3) (b real))
  (vec3+ a (- b)))

(assert (vec3= (vec3 4 6 8) (vec3- (vec3 5 7 9) 1)))

(defmethod vec3- ((a real) (b vec3))
  (vec3 (- a (vec3-x b))
        (- a (vec3-y b))
        (- a (vec3-z b))))

(assert (vec3= (vec3 4 6 8) (vec3- 8 (vec3 4 2 0))))

(defmethod vec3+ ((a real) (b vec3))
  (vec3 (+ a (vec3-x b))
        (+ a (vec3-y b))
        (+ a (vec3-z b))))

(assert (vec3= (vec3 2 3 4) (vec3+ (vec3 1 1 1) (vec3 1 2 3))))

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

(defun vec3-vector (vec3)
  (vector (vec3-x vec3) (vec3-y vec3) (vec3-z vec3)))

(assert (equalp (vec3-vector (vec3 2 3 5)) #(2 3 5)))

(defun vector-vec3 (vector)
  (check-type vector (vector real 3))
  (vec3 (elt vector 0)
        (elt vector 1)
        (elt vector 2)))

(assert (equalp (vector-vec3 #(2 3 5)) (vec3 2 3 5)))

(assert (equalp #1=#(1 2 3) (vec3-vector (vector-vec3 #1#))))
(assert (equalp #1=#(72/3 97 -.1234567890123456) (vec3-vector (vector-vec3 #1#))))



(defstruct rot3
  (φ :φ :type real)
  (θ :θ :type real)
  (ψ :ψ :type real))

(defun rot3 (φ θ ψ)
  (check-type φ real)
  (check-type θ real)
  (check-type ψ real)
  (assert (<= (- pi) φ pi))
  (assert (<= (- (/ pi 2)) θ (/ pi 2)))
  (assert (<= (- pi) ψ pi))
  (make-rot3 :φ φ :θ θ :ψ ψ))

(defun rot3-phi (rot) (rot3-φ rot))
(defun rot3-theta (rot) (rot3-θ rot))
(defun rot3-psi (rot) (rot3-ψ rot))

(assert (= 1/2 (rot3-φ (rot3 1/2 1 0))))
(assert (= 1 (rot3-θ (rot3 1/2 1 0))))
(assert (= 0 (rot3-ψ (rot3 1/2 1 0))))

(defun rot3= (a b &rest more)
  (and (and (= (rot3-φ a) (rot3-φ b))
            (= (rot3-θ a) (rot3-θ b))
            (= (rot3-ψ a) (rot3-ψ b)))
       (if more
           (apply #'rot3= a more)
           t)))

(assert (rot3= (rot3 1 1 1) (rot3 1 1 1)))

(defun mod* (number range)
  (if (minusp number)
      (- (mod (- number) range))
      (mod number range)))

(assert (= 3 (mod* 3 4)))
(assert (= 3 (mod* 7 4)))
(assert (= -3 (mod* -7 4)))

(defun rot3+ (a b &rest more)
  (if more
      (reduce #'rot3+ (cons (list a b) more))
      (rot3 (mod* (+ (rot3-φ a) (rot3-φ b)) (* 2 pi))
            (- (mod* (+ (/ pi 2) (+ (rot3-θ a) (rot3-θ b))) pi) (/ pi 2))
            (mod* (+ (rot3-ψ a) (rot3-ψ b)) (* 2 pi)))))

(assert (rot3= (rot3+ (rot3 0 0 0) (rot3 1 0 2)) (rot3 1 0 2)))
(assert (rot3= (rot3+ (rot3 0 (/ pi 2) 0) (rot3 1 (/ pi 2) 0)) 
               (rot3 1 0 0)))

(defgeneric rot3* (a b &rest more))

(defmethod rot3* :around (a b &rest more)
  (if more
      (reduce #'rot3* (cons (list a b) more))
      (call-next-method)))

(defmethod rot3* ((a rot3) (b real) &rest more)
  (assert (null more))
  (rot3 (mod* (* (rot3-φ a) b) (* 2 pi))
        (mod* (* (rot3-θ a) b) pi)
        (mod* (* (rot3-ψ a) b) (* 2 pi))))

(defmethod rot3* ((a real) (b rot3) &rest more)
  (assert (null more))
  (rot3* b a))

(assert (rot3= (rot3 2 1 0) (rot3* 2 (rot3 1 1/2 0))))
(assert (rot3= (rot3 2 1 0) (rot3* (rot3 1 1/2 0) 2)))

(defun rot3-zerop (rot)
  (and (zerop (rot3-φ rot))
       (zerop (rot3-θ rot))
       (zerop (rot3-ψ rot))))

(assert (rot3-zerop (rot3 0 0 0)))
(assert (not (rot3-zerop (rot3 3 1.12 2))))

(defun rot3° (φ θ ψ)
  (check-type φ (real -180 180))
  (check-type θ (real -90 90))
  (check-type ψ (real -180 180))
  (rot3 (deg-rad φ) 
        (deg-rad θ)
        (deg-rad ψ)))

(defun rot3-degrees (φ θ ψ)	(rot3° φ θ ψ))

(defun rot3-matrix (rot3)
  (let* ((φ (rot3-φ rot3))
         (θ (rot3-θ rot3))
         (ψ (rot3-ψ rot3))
         (sin-φ (sin φ))
         (cos-φ (cos φ))
         (sin-θ (sin θ))
         (cos-θ (cos θ))
         (sin-ψ (sin ψ))
         (cos-ψ (cos ψ)))
    (make-array '(3 3)
                :element-type 'real
                :initial-contents
                (list 
                 (list cos-θ
                       (- (* cos-ψ sin-θ))
                       (* sin-θ sin-ψ))
                 (list (* cos-φ sin-θ)
                       (- (* cos-φ cos-θ cos-ψ) (* sin-φ sin-θ))
                       (- (* -1 cos-ψ sin-φ) (* cos-φ cos-θ sin-ψ)))
                 (list (* sin-φ sin-θ)
                       (+ (* cos-φ sin-ψ) (* cos-θ cos-ψ sin-φ))
                       (- (* cos-φ cos-ψ) (* cos-θ sin-φ sin-ψ)))))))

(defstruct quaternion r i j k)

(defun quaternion-matrix (q)
  (vector (vector (complex (quaternion-r q) (quaternion-i q))
                  (complex (quaternion-j q) (quaternion-k q) ))
          (vector (complex (- (quaternion-j q)) (quaternion-k q))
                  (complex (quaternion-r q) (- (quaternion-i q)) ))))



(defgeneric distance-between (a b))

(defmethod distance-between ((a vec3) (b vec3))
  (sqrt (+ (expt (- (vec3-x a) (vec3-x b)) 2)
           (expt (- (vec3-y a) (vec3-y b)) 2)
           (expt (- (vec3-z a) (vec3-z b)) 2))))



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

(defgeneric body-intersects-p (body-a position-a body-b position-b))

(defmethod body-intersects-p ((a-body spherical-body) (a-center vec3) 
                              (b-body spherical-body) (b-center vec3))
  (> (+ (spherical-body-radius a-body) (spherical-body-radius b-body))
     (distance-between a-center b-center)))

(assert (body-intersects-p (make-instance 'spherical-body :radius 4) (vec3 0 0 0)
                           (make-instance 'spherical-body :radius 4) (vec3 4 0 0)))

;; Explanation of this test case:  These two spheres have an infinitely small distance between  them, but while they may
;; “touch” tangentially, they never actually *intersect* one another.
(assert (not (body-intersects-p (make-instance 'spherical-body :radius 2) (vec3 0 0 0)
                                (make-instance 'spherical-body :radius 2) (vec3 4 0 0))))

(assert (not (body-intersects-p (make-instance 'spherical-body :radius 4) (vec3 0 0 0)
                                (make-instance 'spherical-body :radius 4) (vec3 10 0 0))))

(defmethod body-intersects-p ((a-body rectangular-body) (a-center vec3) 
                              (b-body rectangular-body) (b-center vec3))
  (not (or (let* ((a-center-x (vec3-x a-center))
                  (b-center-x (vec3-x b-center))
                  (a-½-length (/ (rectangular-body-length a-body) 2))
                  (b-½-length (/ (rectangular-body-length b-body) 2))
                  
                  (a-min-x (vec3- a-center-x a-½-length))
                  (a-max-x (vec3+ a-center-x a-½-length))
                  (b-min-x (vec3- b-center-x b-½-length))
                  (b-max-x (vec3+ b-center-x b-½-length)))
             (or (> b-min-x a-max-x)
                 (> a-min-x b-max-x)))
           
           (let* ((a-center-y (vec3-y a-center))
                  (b-center-y (vec3-y b-center))
                  (a-½-width (/ (rectangular-body-width a-body) 2))
                  (b-½-width (/ (rectangular-body-width b-body) 2))
                  
                  (a-min-y (vec3- a-center-y a-½-width))
                  (a-max-y (vec3+ a-center-y a-½-width))
                  (b-min-y (vec3- b-center-y b-½-width))
                  (b-max-y (vec3+ b-center-y b-½-width)))
             (or (> b-min-y a-max-y)
                 (> a-min-y b-max-y)))
           
           (let* ((a-center-z (vec3-z a-center))
                  (b-center-z (vec3-z b-center))
                  (a-½-depth (/ (rectangular-body-depth a-body) 2))
                  (b-½-depth (/ (rectangular-body-depth b-body) 2))
                  
                  (a-min-z (vec3- a-center-z a-½-depth))
                  (a-max-z (vec3+ a-center-z a-½-depth))
                  (b-min-z (vec3- b-center-z b-½-depth))
                  (b-max-z (vec3+ b-center-z b-½-depth)))
             (or (> b-min-z a-max-z)
                 (> a-min-z b-max-z))))))



(defgeneric body-bounding-box (body))
(defgeneric body-bounding-sphere (body))

(defmethod body-bounding-box ((body rectangular-body)) body)
(defmethod body-bounding-sphere ((body spherical-body)) body)



(defun rad-deg (radians)
  (* 360 (mod* (/ radians (* 2 pi)) 1)))

(assert (= 0 (rad-deg 0)))
(assert (= 180 (rad-deg pi)))
(assert (= -180 (rad-deg (- pi))))

(defun deg-rad (degrees)
  (check-type degrees real)
  (* 2 pi (mod* (/ degrees 360) 1)))

(assert (= 0 (deg-rad 0)))
(assert (= (/ pi 2) (deg-rad 90)))
(assert (= (- pi) (deg-rad -180)))

