(in-package :cl-user)
(defpackage tootstest
  (:documentation "This is the main (entry-point/top-level) package used
 to launch the program in various modes of operation.")
  (:use :cl)
  (:import-from :tootstest.config :config)
  (:import-from :clack :clackup)
  (:export #:start #:stop #:fastcgi-entry
           #:entry
           #:fastcgi-entry
           #:start-hunchentoot
           #:power-on-self-test
           #:start-repl
           #:*compiled*
           #:start-swank
           #:write-docs
           #:print-help))
(in-package :tootstest)

(defvar *appfile-path*
  (asdf:system-relative-pathname :tootstest #P"app.lisp")
  "This is the location of app.lisp, which is the anchor-point for the static asset files relative to it.")

(defvar *handler* nil
  "If  a local  (testing?) Hunchentoot  server is  running, its  Clackup
 value will be stored here…")

(defun start (&rest args &key server port debug &allow-other-keys)
  "Start a local Hunchentoot server.

PORT may be a port number .

The server will  be started running on port  5000 on local-loopback-only
addresses  (127.0.0.1  and  ::1).  If an  existing  server  is  running,
a restart will be presented to allow you to kill it (RESTART-SERVER)."
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  "Stop any locally-running Hunchentoot server process, eg, one started by `START'"
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defun print-help ()
  "Prints a short usage summary  to *STANDARD-OUTPUT*. Note that this is
invoked  by calling  the  program  with “help”  as  its first  argument,
explicitly — the default behaviour is to run as a FastCGI server."
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

(defvar *compiled* #.(with-output-to-string (s) (print-object (local-time:now) s))
        "A string representing the (fairly precise) time at which the program was compiled.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *compiled* (with-output-to-string (s) (print-object (local-time:now) s))))

(defun start-repl ()
  "Starts a PREPL REPL."
  (ql:quickload :prepl)
  (funcall (intern "REPL" (find-package :prepl))))

(defun start-swank ()
  "Starts a SWANK server."
  (ql:quickload :swank)
  (funcall (intern "START-SERVER" (find-package :swank))))

(defun inform-declt-of-agplv3 ()
  "Adds the AGPLv3 to the list of licenses for DECLT."
    (let ((licenses (intern "*LICENSES*" (find-package :net.didierverna.declt))))
      (set licenses
           (cons (eval licenses)
                 '((:agplv3
                    "The GNU Affero General Public License"
                    "This  program is  free  software; you  can redistribute  it
and/or  modify it  under  the terms  of the  GNU  Affero General  Public
License as  published by  the Free  Software Foundation;  either version
3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program; if not,  write to the Free Software Foundation,
Inc., 675 Mass Ave, Cambridge, MA 02139, USA."))))))

(defun patch-declt-item/symbol.lisp (source-dir)
  "Patch this file. TODO: File a bug or submit upstream or something."
  (load (merge-pathnames
         (make-pathname :directory '(:relative "src" "lib")
                        :name "net.didierverna.declt.item.symbol"
                        :type "lisp")
         source-dir)))

(defun write-docs ()
  "Write out the documentation in TeΧinfo format using DECLT.

Note that DECLT  is not usually compiled into the  binary by default, so
this  may  have  to  download  DECLT  and/or  its  dependencies  through
Quicklisp when called."
  (format *trace-output* "~& Writing documentation…")
  (ql:quickload :net.didierverna.declt)
  (let ((source-dir (asdf:component-pathname (asdf:find-system :tootstest))))
    (patch-declt-item/symbol.lisp source-dir)
    (inform-declt-of-agplv3)
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (funcall (intern "DECLT" (find-package :net.didierverna.declt))
             :tootstest
             :library-name "Violet Volts: tootstest"
             :texi-file (merge-pathnames #p"doc/tootstest.texi"
                                         source-dir)
             :info-file (merge-pathnames #p "doc/tootstest"
                                         source-dir)
             :license :agplv3
             :declt-notice :short
             :hyperlinks nil
             :introduction (alexandria:read-file-into-string #p"src/static/introduction")
             :conclusion (alexandria:read-file-into-string #p"src/static/conclusion"))))

(defun start-hunchentoot ()
  "Start a Hunchentoot  server via `START' and fall through  into a REPL
to keep the process running."
  (start)
  (print "Hunchentoot server running. Evaluate (TOOTSTEST:STOP) to stop, or exit the REPL.")
  (start-repl))

(defun post-read-version-page ()
  "Power-On-Self-Test: Checks that the server can respond to the version-page query locally."
  (let ((retries 3))
    (tagbody retry-post
       (handler-case
           (return-from post-read-version-page (drakma:http-request "http://localhost:27701/tootstest/version"))
         (usocket:connection-refused-error (c)
           (cond ((minusp (decf retries))
                  (error "Failed POST: Can't connect to local server (after retries)~%~a" c))
                 (t (format *error-output*
                            "~&~a~%Hmm, maybe we need to wait a moment and try that again." c)
                    (force-output *error-output*)
                    (sleep 1)
                    (go retry-post))))))))

(defun power-on-self-test ()
  "Perform some sanity checking as a part of testing.

This testing should  be much more complete  than it really is  — it will
need to be expanded a great deal to increase confidence in these tests."
  (fresh-line)
  (princ "Power-on self-test:")
  (handler-case (start :port 27701)
    (simple-error (c) (if (find-restart :restart-server)
                          (invoke-restart :restart-server)
                          (signal c))))
 ;;; something that appears on the version page, but no error pages.
  (unless (search "Bruce-Robert Fenn Pocock"
                  (post-read-version-page))
    (warn "Failed POST")
    (stop)
    (cl-user::exit :code 27 :abort t :timeout 5)
    nil)
  (princ "Passed POST")
  (fresh-line)
  (stop)
  t)

(defun entry (argv)
  "Top-level  entry-point  for  the  compiled  executable  binary  form.
Dispatches   based   upon   the   single  argument,   expected   to   be
a verb (case-insensitive) from the card-coded table in this function."
  (case (intern (string-upcase (typecase argv
                                 (cons (if (< 1 (length argv))
                                           (second argv)
                                           "FAST-CGI"))
                                 (null "HELP")
                                 (t argv))) :keyword)
    (:fast-cgi (fastcgi-entry))
    (:server (start-hunchentoot))
    (:check (power-on-self-test))
    (:repl (start-repl))
    (:version (print *compiled*))
    (:swank (start-swank))
    (:write-docs (write-docs))
    (otherwise (print-help))))

(defun fastcgi-entry ()
  "This is the entry point for running as a FastCGI program.

Note that it will print out  some HTTP-like garbage when it starts about
“half-working,  if you  see  this.”  If that  actually  appears in  your
browser, it was started as a “not-Fast” CGI program, in error."
  (format t "Content-Type: text/plain~c~c~c~c

 Starting FastCGI is half-working if you see this. But, only half.~%"
          #\Return #\Linefeed #\Return #\Linefeed)
  (format *error-output* "~%FastCGI started~%")
  (let ((ql:*quickload-explain* nil) (ql:*quickload-verbose* nil))
    (clackup *appfile-path* :server :fcgi :port nil :use-thread nil :fd 0)))
