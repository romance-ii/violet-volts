;;; -*- lisp -*-
(defpackage turtar/person
  (:use :cl :oliphaunt
        :turtar/entity)
  (:export #:person))
(in-package :turtar/person)

;;; Person
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



(defstruct personal-name
  (honorific nil)
  (doctorate nil)
  (rank nil)
  (given-name nil)
  (middle-name nil)
  (nickname nil)
  (diminuative nil)
  (surname nil)
  (matronym nil)
  (patronym nil)
  (generation nil)
  (suffix nil))

(defun make-common-name (given-name surname &optional patronym)
  (if patronym
      (make-personal-name :given-name given-name
                          :matronym surname
                          :surname surname
                          :patronym patronym)
      (make-personal-name :given-name given-name
                          :surname surname
                          :patronym surname)))

(defmethod print-object ((name personal-name) s)
  (format s "~{~@[~a~^ ~]~}~@[, ~a~]~@[, ~a~]" 
          (list (or (personal-name-doctorate name)
                    (personal-name-honorific name))
                (personal-name-rank name)
                (personal-name-given-name name)
                (personal-name-middle-name name)
                (when-let (nickname (personal-name-nickname name))
                  (format nil "“~a”" nickname))
                (or (when-let (matronym (personal-name-matronym name))
                      (format nil "~a ~a" matronym (personal-name-patronym name)))
                    (personal-name-patronym name)
                    (personal-name-surname name)))
          (personal-name-generation name)
          (personal-name-suffix name)))

(assert (string= (print-object (make-common-name "John" "Smith") nil) "John Smith"))

(let ((name (make-personal-name :given-name "Bruce-Robert"
                                :matronym "Fenn" :patronym "Pocock")))
  (assert (string= (print-object name nil) "Bruce-Robert Fenn Pocock"))
  (let ((revised (copy-personal-name name)))
    (setf (personal-name-honorific revised) "Rev")
    (assert (string= (print-object revised nil) "Rev Bruce-Robert Fenn Pocock"))))

(let ((name (make-personal-name :given-name "Harry"
                                :patronym "Connick"
                                :generation "Jr")))
  (assert (string= (print-object name nil) "Harry Connick, Jr")))



(defclass person (entity)
  ((name :type personal-name :reader person-name)))

