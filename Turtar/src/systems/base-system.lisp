;;; -*- lisp -*-
(defpackage turtar/system
  (:use :cl :oliphaunt :turtar/entity)
  (:shadowing-import-from #+sbcl sb-introspect function-lambda-list)
  (:export #:proc-system
           #:proc-system-react
           #:proc-system-stats
           #:report-systems
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

(defun proc-system-reactive-components (system)
  (format t "~& No op; TODO. (in ~a)" (proc-system-name system))
  (sleep 10)
  nil)

(defclass system-stats ()
  ((created :reader proc-system-created
            :initform (get-universal-time))
   (stopped :accessor proc-system-stopped
            :initform nil)
   (started :accessor proc-system-started
            :initform nil)
   (components/life :accessor proc-system-components/life
                    :initform 0)
   (transactions/life :accessor proc-system-transactions/life
                      :initform 0)
   (components/last-transaction :accessor proc-system-components/last-transaction
                                :initform nil)
   (last-transaction-start :accessor proc-system-last-transaction-start
                           :initform nil)
   (last-transaction-end :accessor proc-system-last-transaction-end
                         :initform nil)
   (current-transaction-start :accessor proc-system-current-transaction-start
                              :initform nil)
   (journal :reader proc-system-journal
            :initform (make-string-output-stream))
   (signals :reader proc-system-signal-hash
            :initform (make-hash-table :test #'eq))))

(defstruct signal-box
  (condition)
  (signaled (get-internal-real-time)))

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
   (name :initarg :name :reader proc-system-name)
   (stats :initform (make-instance 'system-stats) :reader proc-system-stats)))



(let (universal-time-offset)
  (defun universal-time-offset ()
    (or universal-time-offset
        (setf universal-time-offset(- (get-universal-time)
                                      (/ (get-internal-real-time) 
                                         internal-time-units-per-second))))))

(assert (apply #'= 
               (loop repeat 20 collect (universal-time-offset))))

(defun internal->universal-time (internal-time)
  (+ (/ internal-time internal-time-units-per-second)
     (universal-time-offset)))

;; I suspect there could be a race condition here, but I haven't been able to trip it yet.
(assert (= (round (get-universal-time)) 
           (round (internal->universal-time (get-internal-real-time)))))

(defun format-internal-time (internal-real-time)
  (sb-int:format-universal-time nil (internal->universal-time internal-real-time)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (method '(proc-system-started 
                    proc-system-stopped 
                    proc-system-components/life
                    proc-system-journal
                    proc-system-transactions/life
                    proc-system-components/last-transaction
                    proc-system-last-transaction-start
                    proc-system-last-transaction-end
                    proc-system-current-transaction-start))
    (eval `(defmethod ,method ((proc-system proc-system))
             (,method (proc-system-stats proc-system))))))

(defun proc-system-last-transaction-duration (system)
  (if (proc-system-last-transaction-end system)
      (- (proc-system-last-transaction-end system)
         (proc-system-last-transaction-start system))
      nil))

(defmethod proc-system-signals ((system-stats system-stats))
  (hash-table-values (proc-system-signal-hash system-stats)))
(defmethod proc-system-signals ((proc-system proc-system))
  (mapcar #'copy-object (proc-system-signals (proc-system-stats proc-system))))
(defmethod proc-system-signals-count ((system-stats system-stats))
  (hash-table-count (proc-system-signal-hash system-stats)))
(defmethod proc-system-signals-count ((proc-system proc-system))
  (proc-system-signals (proc-system-stats proc-system)))


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
  (pushnew system-instance *local-systems* 
           :test (compose #'string=)
           :key (compose #'class-name #'class-of)))

(defun unregister-system (system-instance)
  (setf *local-systems*
        (delete-if (lambda (system)
                     (string= (class-name (class-of system-instance))
                              (class-name (class-of system)))) *local-systems*)))

(defmethod initialize-instance :after ((instance proc-system) &key &allow-other-keys)
  (assert (listp (proc-system-selector instance)))
  (unless (slot-boundp instance 'name)
    (setf (slot-value instance 'name) (substitute #\space #\- 
                                                  (string-capitalize (class-name (class-of instance))))))
  (register-system instance))

(let ((*local-systems* nil))
  (let ((null-system (make-instance 'proc-system :selector nil :operator #'identity)))
    (assert (member null-system *local-systems*))
    (unregister-system null-system)
    (assert (not (member null-system *local-systems*)))))

(defun system-thread (system)
  (gethash system *local-systems-running*))

(defun system-running-p (system)
  (when-let (thread (system-thread system))
    (thread-alive-p thread)))

(defmacro with-output-to-journal ((system) &body body)
  `(let ((*standard-output* (proc-system-journal ,system))
         (*error-output* (proc-system-journal ,system))
         (*trace-output* (proc-system-journal ,system)))
     ,@body))

(defun proc-system-react (system)
  (proc-operate system (proc-select-entities system)))

(defmacro with-transaction-timing ((system) &body body)
  `(progn
     (setf (proc-system-current-transaction-start ,system) (get-internal-real-time))
     ,@body
     (setf (proc-system-last-transaction-start ,system) (proc-system-current-transaction-start ,system)
           (proc-system-last-transaction-end ,system) (get-internal-real-time)
           (proc-system-current-transaction-start ,system) nil)))

(defmacro with-signals-trapped ((system) &body body)
  `(handler-case
       (progn ,@body)
     (condition (condition)
       (report-error condition)
       (push (make-instance 'signal-box :condition condition) 
             (proc-system-signals (proc-system-stats ,system))))))

(defun system-thread-runner (system)
  (lambda ()
    (with-output-to-journal (system)
      (until (proc-system-stop-p system)
        (with-signals-trapped (system)
          (with-transaction-timing (system)
            (proc-system-react system)))))))

(defun turtlefy (word)
  (concatenate 'string "〘" word "〙"))



(defun debug-traced-var-if-valid (v f)
  (ignore-errors
   (when (eq :valid
             (sb-di:debug-var-validity v (sb-di:frame-code-location f)))
     (cons (sb-di:debug-var-symbol v)
           (sb-di:debug-var-value v f)))))

(defun debug-vars-in-frame (f)
  (ignore-errors (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun f))))

(defun file-source-of-function-in-debug-frame (f)
  (ignore-errors
   (sb-di:debug-source-namestring
    (sb-di:code-location-debug-source
     (sb-di:frame-code-location f)))))

(defun position-within-file-of-function-in-frame (f)
  (ignore-errors ;;; XXX does not work
   (let ((cloc (sb-di:frame-code-location f)))
     (unless (sb-di:code-location-unknown-p cloc)
       (format nil "tlf~Dfn~D"
               (sb-di:code-location-toplevel-form-offset cloc)
               (sb-di:code-location-form-number cloc))))))

(defun debug-function-name-in-frame (frame)
  (ignore-errors (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))))

(defun debug-file-name-for-function-in-frame (frame)
  (or (when-let (file (file-source-of-function-in-debug-frame frame))
        (concatenate 'string "▭" file))
      "㉄"))

(defun debug-function-start-location (fn)
  (when (typep fn 'sb-di::compiled-debug-fun)
    (let* ((location (sb-di:debug-fun-start-location fn))
           (source (sb-di::code-location-debug-source location)))
      (format nil "~@[~%~4tKind ~a~]~
~@[~%~4tPC ~a~]~
~@[~%~4tSource file ~a~]~
~@[~%~4tRoot number ~:d~]~
~@[~%~4tForm ~a~]~
~@[~%~4tForm Number ~a~]~
~@[~%~4tForm starts at position ~:d in file~]~
~@[~%~4tCreated ~a~]~
~@[~%~4tCompiled ~a~]~
~@[~%~4tBlock ~a~]~
"
              (sb-di::compiled-code-location-kind location)
              (and nil #+todo (sb-di::compiled-code-location-pc location))
              (sb-di::debug-source-namestring source)
              (and nil #+todo (sb-di::debug-source-root-number source))
              (sb-di::debug-source-form source)
              (or (ignore-errors (sb-di::code-location-form-number location)) nil)
              (when-let (form (or (ignore-errors (sb-di::code-location-form-number location)) nil))
                (elt (sb-di::debug-source-start-positions source) form))
              (when-let (time (sb-di::debug-source-created source))
                (sb-int:format-universal-time nil time))
              (when-let (time (sb-di::debug-source-compiled source))
                (sb-int:format-universal-time nil time))
              (or (ignore-errors (sb-di::code-location-debug-block location)) nil)))))

(defun debug-local-var-real-value (value)
  (ignore-errors (progv
                     (mapcar #'car trivial-backtrace::*trivial-backtrace-frame-print-specials*)
                     (mapcar #'cdr trivial-backtrace::*trivial-backtrace-frame-print-specials*)
                   value)))

(defun debug-variable-values (vars frame)
  (remove-if 'not 
             (map 'list (rcurry #'debug-traced-var-if-valid frame)
                  vars)))

(defun print-debug-var-name+value (name value)
  (if-let ((real-value (debug-local-var-real-value value)))
    (format t "~%~4t (~a ~s)" name real-value)
    (format t "~%~4t ~a" name)))

(defun debug-trace-λ-list (λ-list)
  (when λ-list
    (format t "~%Λ List:~8t(")
    (loop
      for firstp = t then nil
      for (name . value) in λ-list
      do (unless firstp (terpri))
      do (if-let ((real-value (debug-local-var-real-value value)))
           (format t "~11t(~a ~a)" name value)
           (format t "~11t~a" name)))
    (format t ")")))

(defun debug-function-decorate-function-name (name)
  (cond
    ((and (consp name)
          (member (first name) '(flet lambda labels))
          (eql :in (third name))) (format nil "(~a ~a) ⋅ ~a"
                                          (if (eql 'lambda (first name)) 'λ (first name))
                                          (or (second name) "()")
                                          (fourth name)))
    ((and (consp name)
          (member (first name) '(flet lambda labels)))
     (cons (if (eql 'lambda (first name)) 'λ (first name)) (rest name)))
    ((consp name) (prin1-to-string name))
    (t name)))

(defun debug-function-simplified-return-type (returns)
  (cond
    ((atom returns) returns)
    ((equalp '(values &optional) returns) "∅")
    ((and (eql 'values (first returns))
          (= 3 (length returns))
          (eql '&optional (third returns))) (second returns))
    (t returns)))

(defun debug-function-decorated (fn name)
  (when-let (fun (sb-di:debug-fun-fun fn))
    (let ((type-seq (sb-introspect:function-type fun)))
      (assert (= 3 (length type-seq)))
      (assert (eql 'function (first type-seq)))
      (format nil "~%(~:(~a~)~@[ ~{~a~^ ~}~]) ⇒ ~a~@[~2%~a~%~]" 
              (debug-function-decorate-function-name name)
              (second type-seq)
              (debug-function-simplified-return-type (third type-seq))
              (documentation name 'function)))))

(defun print-stack-trace ()
  (loop for frame = (or sb-debug:*stack-top-hint*
                        (sb-di:top-frame))
          then (sb-di:frame-down frame)
        for frame-index from 0
        while frame
        do (let* ((fn (sb-di:frame-debug-fun frame))
                  (fn-name (debug-function-name-in-frame frame))
                  (λ-list (debug-variable-values (sb-di:debug-fun-lambda-list fn) frame))
                  (λ-list-names (mapcar #'car λ-list) )) 
             (format t "~3%~:(~[Top Frame~:;~:*~:r Calling Frame~]: ~a~)~
~@[~%~a~]
~6t~a~@[:~a~]~
~@[~% Closure name: ~a~]~
~@[~% Kind: ~a~]~
~@[~% Starting Location: ~a~]~%" 
                     frame-index fn-name
                     (debug-function-decorated fn fn-name)
                     (debug-file-name-for-function-in-frame frame)
                     (position-within-file-of-function-in-frame frame)
                     (sb-di:debug-fun-closure-name fn frame)
                     (sb-di:debug-fun-kind fn)
                     (debug-function-start-location fn))
             (debug-trace-λ-list λ-list)
             (loop for (name . value) in (remove-if (rcurry #'member λ-list-names)
                                                    (debug-variable-values  
                                                     (debug-vars-in-frame frame)
                                                     frame)
                                                    :key #'car)
                   do (print-debug-var-name+value name value)))))

(defun report-error (condition)
  (format t "~&~|~%⁂ An error has been signaled ⁂~%Condition of type ~s~%" (class-name (class-of condition)))
  (terpri)
  (describe-object condition t)
  (terpri)
  (print-stack-trace))


#|
(handler-case
    (eval 'form-to-be-moved)
  (unbound-variable (c)
    (figure-it-out-and-add-to-λ-list)))
|#



(defun start-system (system)
  (handler-case
      (let ((thread (make-thread (system-thread-runner system) :name (turtlefy (proc-system-name system)))))
        (setf (gethash system *local-systems-running*) thread))
    (error (c)
      (format t "~& Unable to start system ~a: ~a" (proc-system-name system) c) 
      (report-error c))))

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
    (setf *overseer* (make-thread #'systems-overseer :name (turtlefy "♠ Systems Overseer ♠")))))

(defmethod turtar:hook-node-bootstrap progn (node)
  (start-systems-overseer-thread))

(defun stop-systems-overseer-thread ()
  (setf *overseer* nil))

(defun system-thread-name (system)
  (thread-name (system-thread system)))

(defun system-status (system)
  (if (system-running-p system) 
      (system-thread-name system)
      "Not Running"))

(defun report-systems ()
  (format t "~&~|
~:(~a~) Systems running locally on node ~a
\(Cluster: ~a; World: ~a)

System Name~40tClass
   Thread~40tLast Dur.~50tComp's.~60tSignals
~{~%~a~40t~a
   ~a~40t~:[—~;~2fs~]~50t~:d~60t~:[—~:;~:d~]~}

Overseer thread~:[ is not running~*~; is running; thread ~a~].

Node ~a on host ~a has ~[no other threads~:;~:*~r other thread~:p~] running,
for a grand total of ~[no~:;~:*~r thread~:p~] in this ~a ~:(~a~) ~a Lisp Image." 
          (length (remove-if-not #'system-running-p *local-systems*))
          (turtar:node-name turtar:*node*)
          (turtar:cluster-name turtar:*cluster*)
          (turtar:world-name turtar:*world*)
          (mapcan (lambda (system) 
                    (list (proc-system-name system)
                          (class-name (class-of system))
                          (system-status system)
                          (proc-system-last-transaction-duration system)
                          (proc-system-components/life system)
                          (length (proc-system-signals system)))) 
                  *local-systems*)
          (overseer-alive-p)
          (when *overseer* (thread-name *overseer*))
          (turtar:node-name turtar:*node*)
          (machine-instance)
          (- (length (all-threads))
             (length (remove-if-not #'system-running-p *local-systems*))
             (if *overseer* 1 0))
          (length (all-threads))
          (lisp-implementation-type)
          (uiop/os:operating-system)
          (machine-type)))
