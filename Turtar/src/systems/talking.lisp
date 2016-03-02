;;; -*- lisp -*-
(defpackage turtar/talking
  (:use :cl :oliphaunt :turtar/system)
  (:export #:talking))
(in-package :turtar/physics)

;;; Talking
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

(defclass talking-system (proc-system)
  ())

(defun talking-update (listener system)
  ())

(defmethod turtar:hook-world-bootstrap progn (world)
  (make-instance 'talking-system
                 :name (concatenate 'string
                                    "Talking system in "
                                    (turtar:world-name world))
                 :operator #'talking-update
                 :selector '(turtar/utterance:language-listener)
                 :filter-not #'turtar/thing:thing-resting-p))


