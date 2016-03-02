(defpackage turtar
  (:use :cl :oliphaunt)
  (:export #:*cluster*
           #:*node*
           #:*world*
           #:cluster
           #:cluster-name
           #:cluster-world
           #:config
           #:hook-cluster-bootstrap
           #:hook-cluster-quiesce
           #:hook-node-bootstrap
           #:hook-node-quiesce
           #:hook-world-bootstrap
           #:node
           #:node-cluster
           #:node-name
           #:node-world
           #:scope-not-configured
           #:scope-not-configured-key-path
           #:turtar
           #:world
           #:world-name
           ))

(in-package :turtar)

(require 'uuid)

(defvar *world*)
(defvar *cluster*)
(defvar *node*)

(defclass world ()
  ((name :initarg :name :reader world-name
         :initform (or (config nil :world :name)
                       "Turtar Test"))))

(defclass cluster ()
  ((name :initarg :name :reader cluster-name
         :initform (format nil "~:(~a~)'s Cluster" (machine-instance)))
   (world :initarg :world :initform *world* :reader cluster-world)))

(defvar *seen-nodes* (make-hash-table :test 'equal))

(defun seen-node-name-p (try)
  (gethash try *seen-nodes*))

(defun seen-node (node)
  (setf (gethash (node-name node) *seen-nodes*) node))

(defvar *seen-clusters* (make-hash-table :test 'equal))

(defun seen-cluster-name-p (try)
  (gethash try *seen-clusters*))

(defun seen-cluster (cluster)
  (setf (gethash (cluster-name cluster) *seen-clusters*) cluster))

(defun make-unique-node-name/numbered (prefix)
  (loop for number from 2
        for try = (format nil "~a ~/oliphaunt:roman-numeral/" prefix number)
        unless (seen-node-name-p try)
          do (return-from make-unique-node-name/numbered try)))

(defun make-unique-node-name ()
  (let ((try (string-capitalize (machine-instance))))
    (if (seen-node-name-p try)
        (make-unique-node-name/numbered try)
        try))) 

(defclass node ()
  ((name :initarg :name :initform (make-unique-node-name) :reader node-name)
   (cluster :initarg :cluster :initform *cluster* :reader node-cluster)))



(defun node-world (node)
  (check-type node node)
  (cluster-world (node-cluster node)))

(defgeneric hook-world-bootstrap (world)
  (:method-combination progn)
  (:method progn ((world world))
    (format t "~&Started world: ~a" (world-name world))))

(defmethod initialize-instance :after ((world world) &key &allow-other-keys)
  (hook-world-bootstrap world)
  (start-cluster world))

(defgeneric hook-cluster-bootstrap (cluster)
  (:method-combination progn)
  (:method progn ((cluster cluster))
    (seen-cluster cluster)
    (format t "~&Started cluster: ~a" (cluster-name cluster))))

(defmethod initialize-instance :after ((cluster cluster) &key &allow-other-keys)
  (hook-cluster-bootstrap cluster)
  (start-node cluster))

(defgeneric hook-node-bootstrap (node)
  (:method-combination progn)
  (:method progn ((node node))
    (seen-node node)
    (format t "~&Started node: ~a" (node-name node))))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (hook-node-bootstrap node))

(define-condition scope-not-configured (error)
  ((scope :initarg :scope :reader scope-not-configured)
   (key-path :initarg :key-path :reader scope-not-configured-key-path)))

(defgeneric config (scope &rest key-path)
  (:method ((scope null) &rest key-path) 
    (declare (ignore key-path)) nil)
  (:method ((scope t) &rest key-path)
    (error 'scope-not-configured :scope scope :key-path key-path)))



(defgeneric hook-cluster-quiesce (cluster)
  (:method-combination progn)
  (:method progn (cluster)
    (world-remove-cluster (cluster-world cluster) cluster)
    (when (eql *cluster* cluster)
      (setf *cluster* nil))
    (format t "~&Quiesced cluster: ~a" (cluster-name cluster))))

(defgeneric hook-node-quiesce (node)
  (:method-combination progn)
  (:method progn (node)
    (when (eql *node* node)
      (setf *node* nil))
    (cluster-remove-node (node-cluster node) node)
    (format t "~&Quiesced node: ~a" (node-name node))))



(defun short-arg-p (arg)
  (and (stringp arg)
       (<= 2 (length arg))
       (char= #\- (elt arg 0))
       (not (char= #\- (elt arg 1)))))

(defun long-arg-p (arg)
  (and (stringp arg)
       (<= 3 (length arg))
       (char= #\- (elt arg 0))
       (char= #\- (elt arg 1))))

(defun parse-short-arg (arg)
  (cons (elt arg 1) (subseq arg 2)))

(assert (equalp (cons #\n "2") (parse-short-arg "-n2")))

(defun parse-long-arg (arg)
  (if (find #\= arg :test #'char=)
      (let ((where= (position #\= arg :test #'char=)))
        (cons (keywordify (subseq arg 2 where=))
              (subseq arg (1+ where=))))
      (cons (keywordify (subseq arg 2)) t)))

(assert (equalp (cons :foo "t") (parse-long-arg "--foo=t")))
(assert (equalp (cons :foo t) (parse-long-arg "--foo")))

(defgeneric accept-input-file (major minor extension namestring)
  (:method ((major string) minor extension namestring)
    (accept-input-file (keywordify major) minor extension namestring))
  (:method (major (minor string) extension namestring)
    (accept-input-file major (keywordify minor) extension namestring))
  (:method (major minor (extension string) namestring)
    (accept-input-file major minor (keywordify extension) namestring))
  (:method (major minor extension namestring)
    (declare (ignore extension))
    (error "Unhandled input file type: ~(~a/~a~) for ~s" major minor namestring)))

(defun dispatch-input-file (namestring)
  (handler-case 
      (let* ((file->mime (uiop/run-program:run-program (list "file" "--mime-type" namestring)))
             (mime-type (string-trim #(#\space) (second (split-sequence #\: file->mime))))
             (extension (last (split-sequence #\. namestring))))
        (destructuring-bind (mime-major mime-minor) (split-sequence #\/ mime-type)
          (accept-input-file (keywordify mime-major) (keywordify mime-minor) (keywordify extension) namestring)))
    (uiop/run-program:subprocess-error (c)
      (error "Could not identify file: ~s~%Subprocess error: ~a" namestring c))))

(defun dispatch-input-directory (namestring)
  (error "Directories are not acceptable for input: ~s" namestring))

(defgeneric accept-command-line-argument (argument parameter)
  (:method ((argument null) parameter)
    (declare (ignore parameter))
    nil)
  (:method ((argument character) parameter)
    (error "Unrecognized command-line argument: -~a" argument))
  (:method ((argument symbol) parameter)
    (error "Unrecognized command-line argument: --~(~a~)" argument))
  (:method ((argument (eql :file)) parameter)
    (dispatch-input-file parameter))
  (:method ((argument (eql :directory)) parameter)
    (dispatch-input-directory parameter))
  (:method (argument parameter)
    (error "Unhandled command-line input ~s ~s" argument parameter)))

(defun parse-one-command-line-arg (arg)
  (cond
    ((null arg) nil)
    ((short-arg-p arg) (parse-short-arg arg))
    ((long-arg-p arg) (parse-long-arg arg))
    ((file-exists-p arg) (cons :file arg))
    ((directory-exists-p arg) (cons :directory arg))
    (t (cons :unparsed arg))))

(defun accept-command-line (&rest arguments)
  (loop for arg in arguments
        for (argument . parameter) = 
                                   (parse-one-command-line-arg arg)
        do (accept-command-line-argument argument parameter)))

(defun process-command-line (arguments)
  (if (and (= 1 (length arguments))
           (listp arguments))
      (apply #'accept-command-line (first arguments))
      (accept-command-line arguments))
  (format t "~& • Done with command-line configuration."))



(defun tell-everyone-it-is-the-end-of-the-world ()
  (dolist (out (list *standard-output*
                     *error-output*
                     *trace-output*
                     *debug-io*
                     *query-io*
                     *terminal-io*))
    (ignore-errors (format out "~6%~|~6% It's the end of the world.~6%"))))

(defun end-of-the-world ()
  (setf *world* nil)
  (tell-everyone-it-is-the-end-of-the-world)
  (error "The world has ended."))



(defun world-cluster-count (world)
  (assert (eql world *world*))
  (if (and (boundp '*cluster*) *cluster*) 1 0))

(defun world-remove-cluster (world cluster)
  (assert (eql world *world*))
  (assert (member cluster (list *cluster* nil)))
  (when (zerop (world-cluster-count world))
    (end-of-the-world)))

(defun cluster-node-count (cluster)
  (assert (eql cluster *cluster*))
  (if (and (boundp '*node*) *node*) 1 0))

(defun cluster-remove-node (cluster node)
  (declare (ignore node))
  (assert (eql cluster *cluster*))
  (when (zerop (cluster-node-count cluster))
    (hook-cluster-quiesce cluster)))

(defun quiesce-node (node)
  (hook-node-quiesce node))

(defun start-node (cluster)
  (setf *node* (make-instance 'node :cluster cluster)))

(defun start-cluster (world)
  (setf *cluster* (make-instance 'cluster :world world)))

(defun start-world ()
  (setf *world* (make-instance 'world)))

(defun turtar-main ()
  (let ((cluster (and (boundp '*cluster*) *cluster*)))
    (if cluster
        (start-node cluster)
        (start-world)))
  (format t "~& • Done with START-WORLD."))

(defun turtar (&rest arguments)
  (let ((old-node (and (boundp '*node*) *node*)))
    (time (progn (time (process-command-line arguments))
                 (time (turtar-main))
                 (format t "~& • Done with bootstrap.")))
    (when old-node
      (quiesce-node old-node))))

