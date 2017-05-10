(in-package :cl-user)
(defpackage tootstest.db
  (:use :cl :alexandria)
  (:import-from :tootstest.config
   :config)
  (:import-from :datafly
   :*connection*
                :connect-cached)
  (:export #:connection-settings
           #:db
           #:with-connection
           #:journal))
(in-package :tootstest.db)

(defvar *db-secrets* nil
  "The credentials used to connect to various databases; hostnames, logins,
 passwords, &c.
 
See `DB', `DB-SECRETS-PATHNAME'")

(defun db-secrets-pathname ()
  "Find  the  pathname of  the  configuration  file  used to  store  the
database credentials to be loaded into `*DB-SECRETS*'

This will be of the form ~/.config/{product-name}/db-secrets.lisp"
  (merge-pathnames
   (make-pathname :directory `(:relative ".config" 
                                         ,(string-downcase :tootstest))
                  :name "db-secrets" :type "lisp")
   (user-homedir-pathname)))

(defun connection-settings (&optional (db :maindb))
  "Obtain the connection credentials to connect to an arbitrary database
server.     possibly    by     reading     them     from    the     file
`DB-SECRETS-PATHNAME' (unless `*DB-SECRETS*' is set)."
  (unless *db-secrets*
    (let ((secret-file (db-secrets-pathname)))
      (if (probe-file secret-file)
          (load secret-file)
          (warn "db-secrets not set; database access will probably fail; ~
tried ~s" secret-file))))
  (or (ignore-errors (cdr (assoc db *db-secrets*)))
      (cdr (assoc db (config :databases)))))

(defun db (&optional (db :maindb))
  "Returns a  connection to  the given database,  possibly making  a new
connection, which  in turn  might require  reading the  credentials from
a file."
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection ((conn) &body body)
  "Evaluates BODY in a context in which `*CONNECTION*' is bound to
database connection identified by CONN"
  `(let ((*connection* ,(if (keywordp conn) `(db ,conn) conn)))
     ,@body))



(defvar *now* nil
  "The time of the current transaction set; consistent through recursive
  updates.")

(defun journal-node (node &optional prior)
  "Insert a node or tree into the journal."
  (etypecase node
    (list (datafly:execute (sxql:insert-into :journal-nodes
                             :event-time *now*
                             :node-type "list"
                             :key nil
                             :value nil
                             :prior prior))
     (let ((parent-id (datafly:retrieve-one (sxql:select (:last-insert-id)))))
       (loop for child in node
             with prior-child
             do (journal-node child prior-child)
             do (setq prior-child child))))
    (hash-table (datafly:execute (sxql:insert-into :journal-nodes
                                   :event-time *now*
                                   :node-type "list"
                                   :key nil
                                   :value nil
                                   :prior prior))
     (let ((parent-id (datafly:retrieve-one (sxql:select (:last-insert-id)))))
       (loop for child in (hash-table-keys node)
             with prior-child
             do (journal-node child prior-child)
             do (setq prior-child child))))
    (atom (datafly:execute (sxql:insert-into :journal-nodes
                             :event-time *now*
                             :node-type (type-of node)
                             :key nil
                             :value nil
                             :prior prior)))))

(defun journal (&rest tree)
  "Insert a node or tree into the journal."
  (with-connection (:journal)
    (let ((*now* (local-time:timestamp-to-universal (local-time:now))))
      (journal-node tree))))
