;;; -*- lisp -*-
(defpackage turtar/thing
  (:use :cl :oliphaunt :turtar
        :turtar/entity
        :turtar/universe)
  (:export #:thing))
(in-package :turtar/thing)

;;; thing
;;; Part of Turtar
;;; Copyright © 2016, Bruce-Robert Fenn Pocock
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.

;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defclass thing (entity)
  ((universe :type universe :initarg :universe :initform turtar/universe:*the-universe* 
             :accessor thing-universe)
   (position :type vec3 :initarg :position :initform (vec3 0 0 0)
             :accessor thing-position)
   (velocity :type vec3 :initarg :velocity :initform (vec3 0 0 0)
             :accessor thing-velocity)
   (acceleration :type vec3 :initarg :acceleration :initform (vec3 0 0 0)
                 :accessor thing-acceleration)
   (jerk :type vec3 :initarg :jerk :initform (vec3 0 0 0)
         :accessor thing-jerk)
   (jounce :type vec3 :initarg :jounce :initform (vec3 0 0 0)
           :accessor thing-jounce)
   (crackle :type vec3 :initarg :crackle :initform (vec3 0 0 0)
            :accessor thing-crackle)
   (pop :type vec3 :initarg :pop :initform (vec3 0 0 0)
        :accessor thing-pop)
   (lock :type vec3 :initarg :lock :initform (vec3 0 0 0)
         :accessor thing-lock)
   (drop :type vec3 :initarg :drop :initform (vec3 0 0 0)
         :accessor thing-drop)
   (position-updated :type real :initform (get-internal-real-time) 
                     :accessor thing-position-updated)
   (mass-g :type real :initarg 1 :initform mass-g
           :accessor thing-mass-g)))

(defun thing-snap (thing) (thing-jounce thing))

(defgeneric thing-position-derivative (thing index)
  (:method ((thing thing) (index (eql 0))) (thing-position thing))
  (:method ((thing thing) (index (eql 1))) (thing-velocity thing))
  (:method ((thing thing) (index (eql 2))) (thing-acceleration thing))
  (:method ((thing thing) (index (eql 3))) (thing-jerk thing))
  (:method ((thing thing) (index (eql 4))) (thing-jounce thing))
  (:method ((thing thing) (index (eql 5))) (thing-crackle thing))
  (:method ((thing thing) (index (eql 6))) (thing-pop thing))
  (:method ((thing thing) (index (eql 7))) (thing-lock thing))
  (:method ((thing thing) (index (eql 8))) (thing-drop thing)))

(defun thing-position-and-derivatives (thing)
  (check-type thing thing)
  (map vector (curry #'thing-position-derivative thing)
       (range 0 8)))

(defun thing-update-position (thing target-real-time)
  (let ((Δτ (- target-real-time (thing-position-updated thing)))
        (derivatives (thing-postition-and-derivatives thing)))
    (loop for derivative from 7 downto 0
       do (incf (aref derivatives derivative) (* Δτ (aref derivatives (1+ derivative)))))
    (make-mutated-copy (thing)
                       :position (aref derivatives 0)
                       :velocity (aref derivatives 1)
                       :acceleration (aref derivatives 2)
                       :jerk (aref derivatives 3)
                       :jounce (aref derivatives 3)
                       :crackle (aref derivatives 3)
                       :pop (aref derivatives 3)
                       :lock (aref derivatives 3)
                       :drop (aref derivatives 3))))


