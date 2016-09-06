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

(defun print-help ()
  (format t "~|
Usage: Run this program with one of these verbs. No verb at all defaults to “fast-cgi”

fast-cgi — run in FastCGI mode under an appropriate server (eg, Apache)
server — start a Hunchentoot server for testing
repl — run a REPL (you might want to rlwrap it)
swank — start a Swank server
write-docs — write out TeΧInfo documentation
help — print this
version — print compilation date-stamp
"))

(defvar *compiled* #.(with-output-to-string (s) (print-object (local-time:now) s)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *compiled* (with-output-to-string (s) (print-object (local-time:now) s))))

(defun start-repl ()
  (ql:quickload :prepl)
  (funcall (intern "REPL" (find-package :prepl))))

(defun start-swank ()
  (ql:quickload :swank)
  (funcall (intern "START-SERVER" (find-package :swank))))

(defun write-docs () 
  (ql:quickload :net.didierverna.declt) 
  (ensure-directories-exist #p"doc/")
  (funcall (intern "DECLT" (find-package :net.didierverna.declt))
           :tootstest
           :library-name "Violet Volts"
           :texi-file #p"doc/violet-volts.texi"
           :tagline "tootstest build"
           :hyperlinks nil
           :introduction (alexandria:read-file-into-string #p"src/static/introduction")))

(defun entry (argv) 
  (case (intern (string-upcase (typecase argv 
                                 (cons (if (< 1 (length argv))
                                           (second argv)
                                           "FAST-CGI"))
                                 (null "HELP")
                                 (t argv))) :keyword)
    (:fast-cgi (fastcgi-entry)) 
    (:server (start))
    (:repl (start-repl))
    (:version (print *compiled*))
    (:swank (start-swank))
    (:write-docs (write-docs)) 
    (otherwise (print-help))))

(defun fastcgi-entry ()
  (format t "Content-Type: text/plain~c~c~c~c
  
  Starting FastCGI is half-working if you see this. But, only half.~%"
          #\Return #\Linefeed #\Return #\Linefeed)
  (format *error-output* "~%FastCGI started~%")
  (let ((ql:*quickload-explain* nil) (ql:*quickload-verbose* nil))
    (clackup *appfile-path* :server :fcgi :port nil :use-thread nil :fd 0)))

