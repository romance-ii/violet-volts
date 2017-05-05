;;;; maintenance.lisp — systems maintenance functions
(in-package :tootstest.web)

(defparameter *maintenance-tasks-performed* nil)

(defroute route-/maintenance "/maintenance/" ()
  (setf (getf (response-headers *response*) :content-type)
        "text/plain;charset=utf-8")
  "You are not the boss of me.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pretty-time (seconds)
    (cond
      ((< seconds 90)
       (format nil "~d second~:p" seconds))
      ((< seconds (* 90 60))
       (format nil "~d minutes" (round seconds 60)))
      ((< seconds (* 36 60 60))
       (format nil "~d hours" (round seconds (* 60 60))))
      (t (format nil "~d days" (round seconds (* 24 60 60)))))))

(defmacro with-maintenance-times ((task-name task-string
                                   start-delay finish-delay)
                                  &body body)
  (let ((task-sym (make-keyword (string task-name)))
        (task-start-sym (make-keyword (concatenate 'string (string task-name)
                                                   (string :-started)))))
    `(block nil
       (setf (getf (response-headers *response*) :content-type)
             "text/plain;charset=utf-8")
       (when-let (last (getf *maintenance-tasks-performed* ,task-sym))
         (when (> last (- (get-universal-time) ,finish-delay))
           (return
             ,(format nil "Task “~a” was performed less than ~a ago."
                      task-string (pretty-time (eval finish-delay))))))
       (when-let (last (getf *maintenance-tasks-performed* ,task-start-sym))
         (when (> last (- (get-universal-time) ,start-delay))
           (return
             ,(format nil "Task “~a” was started less than ~a ago."
                      task-string (pretty-time (eval start-delay))))))
       (prog2
           (setf (getf *maintenance-tasks-performed* ,task-start-sym)
                 (get-universal-time))
           (with-output-to-string (s)
             (let ((*standard-output* s)
                   (*error-output* s)
                   (*trace-output* s))
               
               (handler-case
                   (progn ,@body)
                 (serious-condition (c)
                   (format t "…encountered a serious condition:~%~s~:*~%~a" c)
                   (dolist (kind '(:continue :take-new :accept))
                     (when (find-restart kind)
                       (format t "~&Found a ~a restart; invoking." kind)
                       (invoke-restart kind)))
                   (format t "~&No “safe” restart found; aborting.")))))
         (setf (getf *maintenance-tasks-performed* ,task-sym)
               (get-universal-time))))))

(defmacro define-maintenance-task (label (name start-delay finish-delay)
                                   &body body)
  `(defroute ,(intern (concatenate 'string
                                   (string :route-/maintenance/)
                                   (string label)))
       ,(concatenate 'string
                     "/maintenance/"
                     (string label))
     nil
     (with-maintenance-times (,label
                              ,name
                              ,start-delay ,finish-delay)
       ,@body)))

(define-maintenance-task quicklisp-update
    ("Updating the Quicklisp distribution"
     (* 20 60) (* 24 60 60))
  (ql:update-client)
  (ql:update-all-dists))

(define-maintenance-task hot-reload
    ("Reloading from local sources"
     (* 5 60) (* 30 60))
  (asdf:load-system :tootstest))

(define-maintenance-task buildapp.cgi
    ("Recompiling tootstest.cgi executable"
     (* 20 60) (* 3 60 60))
  (uiop:run-program "make tootstest.cgi"))
