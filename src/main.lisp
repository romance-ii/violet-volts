(in-package :cl-user)
(defpackage tootstest
  (:use :cl)
  (:import-from :tootstest.config :config)
  (:import-from :clack :clackup)
  (:export :start :stop :fastcgi-entry))
(in-package :tootstest)

(defvar *appfile-path*
  (asdf:system-relative-pathname :tootstest #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defun fastcgi-entry (argv)
  (format t "Content-Type: text/plain~c~c~c~c
  
  Starting FastCGI is half-working if you see this. But, only half.~%"
  #\Return #\Linefeed #\Return #\Linefeed)
  (format *error-output* "~%FastCGI started with ARGV=~s~%" argv)
  (let ((ql:*quickload-explain* nil) (ql:*quickload-verbose* nil))
    (clackup *appfile-path* :server :fcgi :port nil :use-thread nil :fd 0)))

