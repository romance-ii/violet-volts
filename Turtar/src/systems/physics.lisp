;;; -*- lisp -*-
(defpackage turtar/physics
  (:use :cl :oliphaunt :turtar/system)
  (:export #:physics
           #:start-physics))
(in-package :turtar/physics)

;;; Physics
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

(defun physics-update (entity system)
  (turtar/entity:with-component-update (entity thing 'turtar/thing:thing)
    (turtar/thing:thing-update-position+rotation thing :target-real-time (physics-now system))))

(defparameter *physics-tick-time* 1/50)

(defclass physics-system (proc-system)
  ((now :type real :accessor physics-now)
   (last-tick :type real :accessor physics-last-tick)
   (Δt :type real :accessor physics-Δt :accessor physics-delta-t)))

(defmethod initialize-instance :after ((system physics-system) &key &allow-other-keys)
  (setf (physics-now system) (get-internal-real-time)
        (physics-last-tick system) (- (get-internal-real-time) *physics-tick-time*)))

(defun next-tick-time (system)
  (+ (physics-last-tick system) *physics-tick-time*))

(defun time-to-next-tick (system)
  (- (get-internal-real-time) (next-tick-time system)))

(defun physics-tick (system)
  (sleep (time-to-next-tick system))
  (setf (physics-last-tick system) (physics-now system)
        (physics-now system) (get-internal-real-time))
  (proc-system-react system))

(defmethod turtar:hook-world-bootstrap progn (world)
  (make-instance 'physics-system
                 :name (concatenate 'string
                                    "Physics system in "
                                    (turtar:world-name world))
                 :operator #'physics-update
                 :selector '(turtar/thing:thing)
                 :reaction #'physics-tick
                 :filter-not #'turtar/thing:thing-resting-p))



