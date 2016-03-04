;;; -*- lisp -*-
(defpackage turtar/system
  (:use :cl :oliphaunt :turtar/entity)
  (:shadowing-import-from #+sbcl sb-introspect function-lambda-list)
  (:export #:proc-system
           #:proc-system-react
           #:start-system
           #:stop-system
           #:start-systems-overseer-thread
           #:stop-systems-overseer-thread))
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
             :initform #'proc-system-reactive-components)
   (stop-p :initform nil :accessor proc-system-stop-p)
   (name :initarg :name :reader proc-system-name)))



(defun proc-select-entities (system)
  (remove-if (or (proc-system-filter-not system) (constantly nil))
             (remove-if-not (or (proc-system-filter system) (constantly t))
                            (remove-duplicates
                             (mapcan #'entities-with-component (proc-system-selector system))
                             :test #'entity-eql))))

(defun sort-by-class (objects-list)
  (stable-sort objects-list #'string< :key (compose #'class-name #'class-of)))

(defun components-list-eql (orig-state new-state)
  (or (eql orig-state new-state)
      (every #'identity (mapcar #'eql
                                (sort-by-class orig-state)
                                (sort-by-class new-state)))))

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

(defun proc-system-react (system)
  (proc-operate system (proc-select-entities system)))

(with-private-local-entities-for-testing
  (let* ((counter 0)
         (null-system (make-instance 'proc-system :selector '(turtar/entity::null-component) 
                                                  :operator (lambda (x)
                                                              (incf counter)
                                                              x))))
    (assert (zerop counter))
    (assert (null (proc-select-entities null-system)))
    (let ((entity (make-instance 'entity)))
      (assert (null (proc-select-entities null-system)))
      (attach-component entity (make-instance 'turtar/entity::null-component))
      (assert (equalp (list entity) (entities-with-component 'turtar/entity::null-component)))
      (assert (equalp (list entity) (proc-select-entities null-system)))
      (assert (zerop counter))
      (proc-operate null-system (proc-select-entities null-system))
      (assert (= 1 counter)))))

(with-private-local-entities-for-testing
  (let* ((e1 (make-instance 'entity))
         (c1 (make-instance 'turtar/entity::null-component))
         (e2 (make-instance 'entity))
         (c2 (make-instance 'turtar/entity::null-component))
         (null-system-1 (make-instance 'proc-system :selector '(turtar/entity::null-component)
                                                    :filter (lambda (e)
                                                              (member c1 (entity-components e)))))
         (null-system-2 (make-instance 'proc-system :selector '(turtar/entity::null-component)
                                                    :filter-not (lambda (e)
                                                                  (member c1 (entity-components e))))))
    (attach-component e1 c1)
    (attach-component e2 c2)
    (assert (equalp (list e1) (proc-select-entities null-system-1)))
    (assert (equalp (list e2) (proc-select-entities null-system-2)))))



(defvar *local-systems* nil
  "The set of systems defined for local execution")
(defvar *local-systems-running* (make-hash-table))

(defun register-system (system-instance)
  (pushnew system-instance *local-systems*))

(defun unregister-system (system-instance)
  (removef *local-systems* system-instance))

(defmethod initialize-instance :after ((instance proc-system) &key &allow-other-keys)
  (assert (listp (proc-system-selector instance)))
  (unless (slot-boundp instance 'name)
    (setf (slot-value instance 'name) (substitute #\space #\- 
                                                  (string-capitalize (class-name (class-of instance))))))
  (register-system instance))

(let ((null-system (make-instance 'proc-system :selector nil :operator #'identity)))
  (assert (member null-system *local-systems*))
  (unregister-system null-system)
  (assert (not (member null-system *local-systems*))))

(defun system-running-p (system)
  (when-let (thread (gethash system *local-systems-running*))
    (thread-alive-p thread)))

(defun system-thread-runner (system)
  (until (proc-system-stop-p system)
    (funcall (proc-system-reaction system) system)))

(defun start-system (system)
  (let ((thread (make-thread (system-thread-runner system) :name (proc-system-name system))))
    (setf (gethash system *local-systems-running*) thread)))

(define-condition thread-stop-forcibly (error) ())

(defun stop-system/forcibly (system)
  (let ((thread (gethash system *local-systems-running*)))
    (remhash system *local-systems-running*)
    (interrupt-thread thread (lambda ()
                               (error 'thread-stop-forcibly)))
    (destroy-thread thread)))

(defun stop-system (system &key politelyp)
  (when (system-running-p system)
    (if politelyp
        (setf (proc-system-stop-p system) t)
        (stop-system/forcibly system))))

(defun start-systems ()
  (dolist (system *local-systems*)
    (unless (system-running-p system)
      (start-system system))))


(defvar *overseer* nil)

(defun overseer-alive-p ()
  (and *overseer*
       (thread-alive-p *overseer*)))

(defun on-overseer-thread-p ()
  (and *overseer*
       (eql *overseer* (current-thread))))

(defun systems-overseer ()
  (sleep 1/100) ; avoid caring about startup race conditions
  (while (on-overseer-thread-p)
    (handler-case
        (progn
          (start-systems)
          (sleep 1/50))
      (condition (c)
        (warn "A condition was signaled to the Systems Overseer:~%~a~%~:*~s" c)))))

(defun start-systems-overseer-thread ()
  (unless (overseer-alive-p)
    (setf *overseer* (make-thread #'systems-overseer :name "Systems Overseer"))))

(defun stop-systems-overseer-thread ()
  (setf *overseer* nil))
