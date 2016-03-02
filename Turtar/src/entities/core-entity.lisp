;;; -*- lisp -*-
(defpackage turtar/entity
  (:use :cl :oliphaunt :turtar)
  (:export #:entity
           #:entity-eql
           #:entity-equal))
(in-package :turtar/entity)

;;; core-entity

;;; Part of Turtar

;;; Copyright © 2016, Bruce-Robert Fenn Pocock

;;; This program is free  software: you can redistribute it and/or  modify it under the terms of  the GNU Affero General
;;; Public License as  published by the Free Software Foundation,  either version 3 of the License,  or (at your option)
;;; any later version.

;;; This program is distributed in  the hope that it will be useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of  MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU  Affero General Public  License for
;;; more details.

;;; You should  have received a  copy of  the GNU Affero  General Public License  along with  this program. If  not, see
;;; <http://www.gnu.org/licenses/>.

(require :uuid)

(defclass entity ()
  (uuid :reader entity-id :initform (uuid:make-v4-uuid)))

(defun entity-eql (a b)
  (equal (entity-id a) (entity-id b)))

(defgeneric entity-equal (a b))

(defmacro make-mutated-copy ((base-instance) &rest slot-value-pairs &key &allow-other-keys)
  `(apply #'make-instance
          (class-of ,base-instance)
          ,@slot-value-pairs
          (loop for slot in (closer-mop:class-slots (class-of ,base-instance))
             when (not (member (closer-mop:slot-name slot) )))))
(error todo)
