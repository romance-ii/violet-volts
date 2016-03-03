;;; -*- lisp -*-
(defpackage turtar/system
  (:use :cl :oliphaunt :turtar/entity)
  (:shadowing-import-from #+sbcl sb-introspect function-lambda-list)
  (:export #:proc-system #:start-system))
(in-package :turtar/system)

;;; Base Processing System Class
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

(defun proc-system-reactive-components ())

(defclass proc-system ()
  ((selector :initarg :selector :reader proc-system-selector)
   (filter :initarg :filter :reader proc-system-filter
           :initform nil)
   (filter-not :initarg :filter-not :reader proc-system-filter-not
               :initform nil)
   (operator :initarg :operator :reader proc-system-operator)
   (reaction :initarg :reaction :reader proc-system-reaction
             :initform #'proc-system-reactive-components)))

(defmethod initialize-instance :after ((instance proc-system) &key &allow-other-keys)
  (assert (consp (proc-system-selector instance))))

(defun proc-select-entities (system)
  (remove-duplicates (mapcan #'entities-with-component (proc-system-selector system))
                     :test #'entity-eql))

(defun components-list-eql (orig-state new-state)
  (or (eql orig-state new-state)
      (every #'identity (mapcar #'eql
                                (stable-sort orig-state #'string< :key (compose #'class-name #'class-of))
                                (stable-sort new-state #'string< :key (compose #'class-name #'class-of))))))

(defun proc-operate/build-change-set (operator entities)
  (let ((change-set nil))
    (dolist (entity entities)
      (let* ((orig-state (entity-components entity))
             (new-state (funcall operator orig-state)))
        (unless (components-list-eql orig-state new-state)
          (push (cons (entity-id entity) new-state) change-set))))
    change-set))

(defun transaction-make-changes (change-set)
  (when change-set
    (with-entity-transaction-locked ()
                                    (dolist (pair change-set)
                                      (destructuring-bind (entity-id components) pair
                                        (if components
                                            (update-entity-components entity-id components)
                                            (delete-entity entity-id)))))))

(defun proc-operate (system entities)
  (transaction-make-changes (proc-operate/build-change-set (proc-system-operator system) entities)))

(let* ((counter 0)
       (null-system (make-instance 'proc-system :selector '(turtar/entity::null-component) 
                                   :operator (lambda (x)
                                               (incf counter)
                                               x)))
       (turtar/entity::*local-entities* (turtar/entity::make-weak-table)))
  (assert (zerop counter))
  (assert (null (proc-select-entities null-system)))
  (let ((entity (make-instance 'entity)))
    (assert (null (proc-select-entities null-system)))
    (attach-component entity (make-instance 'turtar/entity::null-component))
    (assert (equalp (list entity) (entities-with-component 'turtar/entity::null-component)))
    (assert (equalp (list entity) (proc-select-entities null-system)))
    (assert (zerop counter))
    (proc-operate null-system (proc-select-entities null-system))
    (assert (= 1 counter))))


