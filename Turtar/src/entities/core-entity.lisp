;;; -*- lisp -*-
(defpackage turtar/entity
  (:use :cl :oliphaunt)
  (:export #:attach-component
           #:component
           #:component-equal
           #:component-version
           #:component-hash
           #:copy-object
           #:delete-entity
           #:entities-with-component
           #:entity
           #:entity-component
           #:entity-components
           #:entity-components/sorted
           #:entity-eql
           #:entity-equal
           #:entity-has-component-p
           #:entity-id
           #:mutate
           #:update-entity-components
           #:with-component-update
           #:with-entity-transaction-locked
           #:with-private-local-entities-for-testing))
(in-package :turtar/entity)

;;; core-entity
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

(require 'uuid)
(require 'closer-mop)



(defun make-uuid-string ()
  (princ-to-string (uuid:make-v4-uuid)))

(defclass entity ()
  ((entity-uuid :reader entity-id :initform (make-uuid-string))
   (direct-components :reader entity-direct-components :initform (make-hash-table :test 'eql))))

(defconstant +component-hash-size+ 4)

(defclass component ()
  ((component-version :reader component-version :initform 0 :type fixnum)
   (component-hash :type (list 'vector 'fixnum +component-hash-size+) :reader component-hash
                   :initform (make-array +component-hash-size+ :element-type 'fixnum 
                                         :initial-element 0))))

(defclass null-component (component) ())



(defgeneric entity-components (entity) 
  (:method-combination nconc))

(defmethod entity-components nconc ((entity entity))
  (if-let (list (hash-table-values (entity-direct-components entity)))
    list
    (values)))

(let ((entity (make-instance 'entity)))
  (assert (equalp '() (entity-components entity)))
  (assert (equal (entity-id entity) (entity-id entity))))

(defmethod print-object ((entity entity) s)
  (format s "#<Entity ~a~@[ with components:~%~{ • ~a~^~%~}~]>"
          (entity-id entity)
          (entity-components entity)))

(defmethod print-object ((component component) s)
  (format s "#<~:(~a~) (version ~a, hash ~{~x:~})>"
          (class-name (class-of component))
          (component-version component)
          (coerce (component-hash component) 'list)))

(defun entity-components/sorted (entity)
  (stable-sort (mapcar (compose #'class-name #'class-of) (entity-components entity))
               #'string<))

(defgeneric component-equal (a b)
  (:method ((a component) (b component))
    (and (eql (class-of a) (class-of b))
         (= (slot-value a 'component-version) (slot-value b 'component-version))
         (equalp (slot-value a 'component-hash) (slot-value b 'component-hash))))
  (:method ((a null-component) (b null-component)) t)
  (:method (a b) nil))

(let ((c (make-instance 'component))
      (d (make-instance 'component)))
  (assert (not (eql c d)))
  (assert (component-equal c d)))

(defgeneric entity-component (entity component-class) 
  (:method-combination or))

(defmethod entity-component or (entity (component-class symbol))
  (gethash component-class (entity-direct-components entity)))

(defgeneric entity-has-component-p (entity component-class)
  (:method-combination or))

(defmethod entity-has-component-p or (entity (component-class symbol))
  (when (gethash component-class (entity-direct-components entity))
    t))

(defun attach-component (entity component)
  (let ((component-class (class-name (class-of component))))
    (if-let (found (entity-component entity component-class))
      (error 'component-already-attached :entity entity :new-component component :old-component found)
      (setf (gethash component-class (slot-value entity 'direct-components)) component))))

(let ((entity (make-instance 'entity))
      (a (make-instance 'component))
      (b (make-instance 'component)))
  (attach-component entity a)
  (entity-components entity)
  (assert (member a (entity-components entity) :test #'component-equal))
  (assert (component-equal a (entity-component entity 'component)))
  (assert (handler-case
              (progn (attach-component entity b) nil)
            (component-already-attached (c) (declare (ignore c)) t))))

(let ((entity (make-instance 'entity)))
  (assert (null (entity-component entity 'null-component)))
  (assert (not (entity-has-component-p entity 'null-component)))
  (let ((n (make-instance 'null-component)))
    (attach-component entity n)
    (assert (entity-has-component-p entity 'null-component))
    (assert (eql n (entity-component entity 'null-component)))))

(define-condition entity-component-error (error) 
  ((entity :initarg :entity :reader entity-component-error-entity))
  (:report (lambda (c s) 
             (format s "An error occurred with ~:[an entity:component relationship~
~;a component related to entity ~:*~a~]"
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


(define-condition component-not-attached (entity-component-error)
  ((component-class :initarg :component-class :reader component-not-attached-component-class))
  (:report (lambda (c s) 
             (format s "There is not a component of class ~s attached to ~:[that entity~;entity ~:*~a~]"
                     (component-not-attached-component-class c)
                     (entity-component-error-entity c)))))

(defgeneric delete-component (entity component &key if-not-exists))

(defun error-or-ignore (switch error-class &rest error-initargs)
  (check-type switch (member :error :ignore))
  (ecase switch
    (:error (apply #'error error-class error-initargs))
    (:ignore nil)))

(assert (null (error-or-ignore :ignore 'error "Assertion failure…")))
(assert (handler-case
            (progn (error-or-ignore :error 'warning "Foo") nil)
          (warning (c) (declare (ignore c)) t)))

(defmethod delete-component (entity (component-class symbol) &key (if-not-exists :error))
  (assert (member if-not-exists '(:error :ignore)))
  (if-let (found (entity-component entity component-class))
    (remhash component-class (slot-value entity 'direct-components))
    (error-or-ignore if-not-exists 'component-not-attached :entity entity :component-class component-class)))

(let ((entity (make-instance 'entity)))
  (attach-component entity (make-instance 'null-component))
  (delete-component entity 'null-component)
  (assert (null (delete-component entity 'null-component :if-not-exists :ignore)))
  (assert (handler-case
              (progn (delete-component entity 'null-component) nil)
            (component-not-attached (c)
              (assert (eql 'null-component (component-not-attached-component-class c)))
              t))))



(defun entity-eql (a b)
  (let ((eql (eql a b))
        (uuid= (equal (entity-id a) (entity-id b))))
    (or (and eql uuid=)
        (and (not eql) (not uuid=))
        (error "EQL ≠ UUID= — error ? (EQL ~s; UUID= ~s)"
               eql uuid=))
    eql))

(defun entity-components-equal (a b classes)
  (check-type a entity)
  (check-type b entity)
  (check-type classes list)
  (or (zerop (length classes))
      (every #'identity (mapcar #'component-equal 
                                (mapcar (curry #'entity-component a) classes)
                                (mapcar (curry #'entity-component b) classes)))))

(defun entity-equal (a b)
  (let ((sorted-a (entity-components/sorted a)))
    (or (eql a b)
        (and (equalp sorted-a
                     (entity-components/sorted b))
             (entity-components-equal a b sorted-a)))))

(let ((a (make-instance 'entity))
      (b (make-instance 'entity)))
  (assert (not (entity-eql a b)))
  (assert (entity-eql a a))
  (assert (entity-equal a b))
  (assert (entity-equal a a)))



(defun component-increment-version (new-instance)
  (let ((version (1+ (slot-value new-instance 'component-version))))
    (setf (slot-value new-instance 'component-version) version)
    (setf (aref (slot-value new-instance 'component-hash) (mod version +component-hash-size+))
          (sxhash new-instance))))

(defgeneric copy-object (object)
  (:method ((object cons)) (copy-list object))
  (:method ((object array)) (copy-array object))
  (:method ((object string)) (copy-seq object))
  (:method ((object number)) object)
  (:method ((object symbol)) object)
  (:method ((object null)) nil))

(assert (equalp '(1 2 3 4 5) (copy-object '(1 2 3 4 5))))
(assert (not (eql (list 1 2 3 4 5) (copy-object (list 1 2 3 4 5)))))
(assert (equal "foo" (copy-object "foo")))
(assert (not (eql (concatenate 'string "f" "o" "o") (copy-object "foo"))))
(assert (= 42 (copy-object 42)))
(assert (= #c(-4000/17 22.050) (copy-object #c(-4000/17 22.050))))
(assert (null (copy-object nil)))

(defmethod copy-object ((base-instance standard-object))
  (let ((new-instance (make-instance (class-of base-instance))))
    (loop
       for slot-definition in (closer-mop:class-slots (class-of base-instance))
       for slot-name = (closer-mop:slot-definition-name slot-definition)
       do (when (slot-boundp base-instance slot-name)
            (setf (slot-value new-instance slot-name) (slot-value base-instance slot-name))))
    new-instance))


(defmethod copy-object :around ((base-instance component))
  (let ((new-instance (call-next-method)))
    (component-increment-version new-instance)
    new-instance))

(let ((c (make-instance 'null-component)))
  (assert (component-equal c (copy-object c)))
  (assert (/= (component-version c) (component-version (copy-object c)))))

(defmethod copy-object ((base-instance entity))
  (let ((new-instance (make-instance 'entity)))
    (dolist (component (entity-components base-instance))
      (attach-component new-instance (copy-object component)))
    new-instance))

(let ((entity (make-instance 'entity)))
  (assert (not (entity-eql entity (copy-object entity))))
  (assert (entity-equal entity (copy-object entity))))




(defgeneric mutate (base-instance &rest slot-value-pairs &key &allow-other-keys)
  (:documentation "Given a CLOS object BASE-INSTANCE, create  a newly-constructed object whose slot-values are the same,
except that the slots named  by the plist SLOT-VALUE-PAIRS have instead the new values  given. Unbound slots will remain
unbound in the new instance, unless they are initialized by `MAKE-INSTANCE' (eg, by :INITFORM or `INITIALIZE-INSTANCE').
Returns the newly-constructed object instance without altering BASE-INSTANCE."))

(defmethod mutate ((base-instance standard-object) &rest slot-value-pairs &key &allow-other-keys)
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
        new-instance)
      (copy-object base-instance)))

(eval-when (:load-toplevel)
  (defpackage turtar/entity/test (:use :cl :oliphaunt :turtar/entity))
  (in-package :turtar/entity/test)
  (defclass simple-class ()
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
      (assert (not (slot-boundp mutated 'unbound-slot)) ()))
    (let ((unmutated (mutate original)))
      (assert (= 42 (slot-value unmutated 'slot-a)) ())
      (assert (equal "monkey" (slot-value unmutated 'slot-b)) ())
      (assert (not (slot-boundp unmutated 'unbound-slot)) ())))
  (in-package :turtar/entity)
  (delete-package :turtar/entity/test))

(defmethod mutate :around ((base-instance component) &rest slot-value-pairs &key &allow-other-keys)
  (declare (ignore slot-value-pairs))
  (let ((new-instance (call-next-method)))
    (component-increment-version new-instance)
    new-instance))

(let ((entity (make-instance 'entity))
      (component (make-instance 'null-component)))
  (attach-component entity component)
  (assert (entity-equal entity (copy-object entity)))
  (assert (not (entity-eql entity (copy-object entity)))))



(defun make-weak-table ()
  (make-hash-table :test 'eql :weakness :value))

(defvar *local-entities* (make-weak-table))

(defmethod initialize-instance :after ((instance entity) &key &allow-other-keys)
  (setf (gethash (entity-id instance) *local-entities*) instance))

(defmethod delete-entity ((entity entity))
  (remhash (entity-id entity) *local-entities*))

(defun local-entities ()
  (hash-table-values *local-entities*))

(let ((*local-entities* (make-weak-table)))
  (assert (zerop (hash-table-count *local-entities*)))
  (let ((entity (make-instance 'entity)))
    (assert (= 1 (hash-table-count *local-entities*)))
    (assert (equalp (list entity) (local-entities)))
    (delete-entity entity)
    (assert (zerop (hash-table-count *local-entities*)))
    (assert (emptyp (local-entities)))))

(defgeneric entities-with-component (component-class &key remove-if remove-if-not
                                                       &allow-other-keys)
  (:method-combination nconc))

(defmethod entities-with-component :around (component-class &key
                                                              remove-if 
                                                              remove-if-not
                                                              &allow-other-keys)
  (if (or remove-if remove-if-not)
      (funcall (compose (if remove-if
                            (curry #'remove-if remove-if)
                            #'identity)
                        (if remove-if-not
                            (curry #'remove-if-not remove-if-not)
                            #'identity))
               (call-next-method))
      (call-next-method)))

(defmethod entities-with-component nconc ((component-class symbol) &key &allow-other-keys)
  (remove-if-not (rcurry #'entity-has-component-p component-class) (local-entities)))

(let* ((*local-entities* (make-weak-table))
       (e1 (make-instance 'entity))
       (e2 (make-instance 'entity)))
  (assert (equalp (stable-sort (list e1 e2) #'string< :key #'entity-id)
                  (stable-sort (local-entities) #'string< :key #'entity-id)))
  (assert (equalp nil (entities-with-component 'null-component)))
  
  (attach-component e1 (make-instance 'null-component))
  (assert (entity-has-component-p e1 'null-component))
  (assert (equalp (list e1) (entities-with-component 'null-component)))
  (assert (equalp nil (entities-with-component 'null-component :remove-if (constantly t))))
  (assert (equalp (list e1) (entities-with-component 'null-component :remove-if-not (constantly t))))
  
  (attach-component e2 (make-instance 'null-component))
  (assert (equalp (stable-sort (list e1 e2) #'string< :key #'entity-id)
                  (stable-sort (local-entities) #'string< :key #'entity-id)))
  (assert (equalp (stable-sort (list e1 e2) #'string< :key #'entity-id)
                  (stable-sort (entities-with-component 'null-component)
                               #'string< :key #'entity-id)))
  (assert (equalp (list e2)
                  (entities-with-component 'null-component :remove-if (curry #'eql e1))))
  (assert (equalp (list e1)
                  (entities-with-component 'null-component :remove-if-not (curry #'eql e1)))))



(defvar entity-transaction-lock% (make-lock "Entity Transaction Lock"))
(defmacro with-entity-transaction-locked (() &body body)
  `(with-lock-held (entity-transaction-lock%)
     ,@body))

(defun replace-component (entity component)
  (delete-component entity (class-name (class-of component)))
  (attach-component entity component))

(let ((entity (make-instance 'entity))
      (c1 (make-instance 'null-component))
      (c2 (make-instance 'null-component)))
  (attach-component entity c1)
  (assert (eql c1 (entity-component entity 'null-component)))
  (replace-component entity c2)
  (assert (eql c2 (entity-component entity 'null-component))))

(defun update-entity-components (entity-id components)
  (let ((entity (gethash entity-id *local-entities*)))
    (dolist (component components)
      (replace-component entity component))))

(defmacro with-private-local-entities-for-testing (&body body)
  `(let ((*local-entities* (make-weak-table)))
     ,@body))

(defmacro with-component-update ((entity component-instance component-class) &body body)
  (let ((orig-components (gensym (format nil "~s-COMPONENTS-" entity)))
        (component-instance-version (gensym (format nil "~s-VERSION-" component-instance))))
    `(let* ((,orig-components (entity-components ,entity))
            (,component-instance (entity-component ,entity ,component-class))
            (,component-instance-version (component-version ,component-instance)))
       (progn ,@body)
       (if (> (component-version ,component-instance) ,component-instance-version)
           (substitute-if ,component-instance (lambda (component)
                                                (eql (class-of component) (class-of ,component-instance)))
                          ,orig-components)
           ,orig-components))))

