;;; -*- lisp -*-
(defpackage turtar/entity
  (:use :cl :oliphaunt)
  (:export #:copy-object
           #:entity
           #:entity-eql
           #:entity-equal
           #:mutate))
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

(require 'uuid)
(require 'closer-mop)



(defclass entity ()
  ((entity-uuid :reader entity-id :initform (uuid:make-v4-uuid))
   (direct-components :reader entity-direct-components :initform (make-hash-table :test 'eql))))

(defconstant +component-hash-size+ 4)

(defclass component ()
  ((component-version :reader component-version :initform 0 :type fixnum)
   (component-hash :type (list 'vector 'fixnum +component-hash-size+) :reader component-hash
                   :initform (make-array +component-hash-size+ :element-type 'fixnum 
                                         :initial-element most-negative-fixnum))))



(defgeneric entity-components (entity) 
  (:method-combination nconc))

(defmethod entity-components nconc ((entity entity))
  (if-let (list (hash-table-values (entity-direct-components entity)))
    (print list)
    (values)))

(let ((entity (make-instance 'entity)))
  (assert (equalp '() (entity-components entity))))

(defmethod print-object ((entity entity) s)
  (format s "#<An Entity (ID: ~a)~@[ with components:~{~% • ~a~}~%~]>"
          (entity-id entity)
          (entity-components entity)))

(defun sorted-component-classes (entity)
  (sort (mapcar (compose #'class-name #'class-of) (entity-components entity))
        #'string< :key #'symbol-name))

(defgeneric entity-component (entity component-class) 
  (:method-combination or))

(defmethod entity-component or (entity (component-class symbol))
  (gethash component-class (entity-direct-components entity)))

(defgeneric component-equal (a b)
  (:method ((a component) (b component))
    (and (eql (class-of a) (class-of b))
         (= (slot-value a 'component-version) (slot-value b 'component-version))
         (equalp (slot-value a 'component-hash) (slot-value b 'component-hash))))
  (:method (a b) nil))

(let ((c (make-instance 'component))
      (d (make-instance 'component)))
  (assert (not (eql c d)))
  (assert (component-equal c d)))

(define-condition entity-component-error (error) 
  ((entity :initarg :entity :reader entity-component-error-entity))
  (:report (lambda (c s) 
             (format s "An error occurred with ~:[an entity:component relationship~;a component related to entity ~:*~a~]"
                     (entity-component-error-entity c)))))

(define-condition component-already-attached (entity-component-error)
  ((new-component :initarg :new-component :reader component-already-attached-new-component)
   (old-component :initarg :old-component :reader component-already-attached-old-component))
  (:report (lambda (c s) 
             (format s "Can not attach component ~a to ~:[an unknown entity~;entity ~:*~a~];
 there is already a component of class ~s attached (~a)"
                     (component-already-attached-new-component c)
                     (entity-component-error-entity c)
                     (class-name (class-of (component-already-attached-old-component c)))
                     (component-already-attached-old-component c)))))

(defun attach-component (entity component)
  (let ((component-class (class-name (class-of component))))
    (if-let (found (entity-component entity component-class))
      (error 'component-already-attached :entity entity :new-component component :old-component found)
      (setf (gethash component-class (slot-value entity 'direct-components)) component))))

(let ((entity (make-instance 'entity))
      (a (make-instance 'component))
      (b (make-instance 'component)))
  (attach-component entity a)
  (print (entity-components entity))
  (assert (member a (entity-components entity) :test #'component-equal))
  (assert (component-equal a (entity-component entity 'component)))
  (assert (handler-case
              (progn (attach-component entity b) nil)
            (component-already-attached (c)
              (declare (ignore c))
              t))))

(define-condition component-not-attached (entity-component-error)
  ((component-class :initarg :component :reader component-not-attached-component-class))
  (:report (lambda (c s) 
             (format s "There is not a component of class ~s attached to ~:[that entity~;entity ~:*~a~]"
                     (component-not-attached-component-class c)
                     (entity-component-error-entity c)))))

(defgeneric delete-component (entity component))

(defmethod delete-component (entity (component component))
  (let ((component-class (class-name (class-of component))))
    (delete-component entity component-class)))

(defmethod delete-component (entity (component-class symbol))
  (if-let (found (entity-component entity component-class))
    (error 'component-not-attached :entity entity :component found)
    (remhash component-class (slot-value entity 'direct-components))))



(defun entity-eql (a b)
  (uuid:uuid= (entity-id a) (entity-id b)))

(defun entity-components-equal (a b classes)
  (check-type a entity)
  (check-type b entity)
  (check-type classes list)
  (or (zerop (length classes))
      (every #'identity (mapcar #'component-equal 
                                (mapcar (curry #'entity-component a) classes)
                                (mapcar (curry #'entity-component b) classes)))))

(defun entity-equal (a b)
  (let ((sorted-a (sorted-component-classes a)))
    (or (eql a b)
        (and (equalp sorted-a
                     (sorted-component-classes b))
             (not (emptyp sorted-a))
             (entity-components-equal a b sorted-a)))))

(let ((a (make-instance 'entity))
      (b (make-instance 'entity)))
  (assert (not (entity-eql a b)))
  (assert (entity-eql a a))
  (assert (not (entity-equal a b)))
  (assert (entity-equal a a)))

(defun component-increment-version (new-instance)
  (let ((version (1+ (slot-value new-instance 'component-version))))
    (setf (slot-value new-instance 'component-version) version)
    (setf (aref (slot-value new-instance 'component-hash) (mod version +component-hash-size+))
          (sxhash new-instance))))

(defmethod copy-object :around ((base-instance component))
  (let ((new-instance (call-next-method)))
    (component-increment-version new-instance)
    new-instance))

(defmethod copy-object ((base-instance standard-object))
  (let ((new-instance (make-instance (class-of base-instance))))
    (loop
       for slot-definition in (closer-mop:class-slots (class-of base-instance))
       for slot-name = (closer-mop:slot-definition-name slot-definition)
       do (when (slot-boundp base-instance slot-name)
            (setf (slot-value new-instance slot-name) (slot-value base-instance slot-name))))
    new-instance))

(defun mutate (base-instance &rest slot-value-pairs &key &allow-other-keys)
  "Given a CLOS object BASE-INSTANCE, create a newly-constructed  object whose slot-values are the same, except that the
slots named by  the plist SLOT-VALUE-PAIRS have instead the  new values given. Unbound slots will  remain unbound in the
new  instance, unless  they are  initialized by  MAKE-INSTANCE. Returns  the newly-constructed  object instance  without
altering BASE-INSTANCE."
  (if slot-value-pairs
      (let ((mutated-keys (oliphaunt:plist-keys slot-value-pairs))
            (new-instance (make-instance (class-of base-instance))))
        (loop
           for slot-definition in (closer-mop:class-slots (class-of base-instance))
           for slot-name = (closer-mop:slot-definition-name slot-definition)
           do (if-let (mutater-initarg (some (rcurry #'find  mutated-keys)
                                             (closer-mop:slot-definition-initargs slot-definition)))
                (setf (slot-value new-instance slot-name) (getf slot-value-pairs mutater-initarg))
                (when (slot-boundp base-instance slot-name)
                  (setf (slot-value new-instance slot-name) (slot-value base-instance slot-name)))))
        (component-increment-version new-instance)
        new-instance)
      (copy-object base-instance)))

(progn
  (defpackage turtar/entity/test (:use :cl :oliphaunt :turtar/entity))
  (in-package :turtar/entity/test)
  (defclass simple-class (entity)
    ((slot-a :initarg :a) (slot-b :initarg :b) (unbound-slot :initarg :ub)))
  (let ((original (make-instance 'simple-class :a 42 :b "monkey")))
    (assert (= 42 (slot-value original 'slot-a)) ())
    (assert (equal "monkey" (slot-value original 'slot-b)) ())
    (assert (not (slot-boundp original 'unbound-slot)) ())
    (let ((mutated (mutate original :b :horsey)))
      (assert (= 42 (slot-value original 'slot-a) (slot-value mutated 'slot-a)) ())
      (assert (equal "monkey" (slot-value original 'slot-b)) ())
      (assert (equal :horsey (slot-value mutated 'slot-b)) () (slot-value mutated 'slot-b))
      (assert (not (slot-boundp original 'unbound-slot)) ())
      (assert (not (slot-boundp mutated 'unbound-slot)) ())
      (assert (entity-eql original mutated) ())
      (assert (not (entity-equal original mutated)) ()))
    (let ((unmutated (mutate original)))
      (assert (= 42 (slot-value unmutated 'slot-a)) ())
      (assert (equal "monkey" (slot-value unmutated 'slot-b)) ())
      (assert (not (slot-boundp unmutated 'unbound-slot)) ())
      (assert (entity-eql original unmutated) ())
      (assert (not (entity-equal original unmutated)) ())))
  (delete-package :turtar/entity/test))
