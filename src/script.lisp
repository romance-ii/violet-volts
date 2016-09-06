(eval-when (:compile-toplevel :load-toplevel)
  (unless (find-package :jscl)
    (load (asdf:system-relative-pathname :tootstest
                                         "src/lib/jscl/jscl" :type "lisp"))))
(defpackage :tootstest.js (:use :jscl :cl))
(in-package :tootstest.web)
(syntax:use-syntax :annot)

(defparameter *mesh-dir* (asdf:system-relative-pathname :tootstest "src/mesh/"))
(defparameter *work-dir*
  (merge-pathnames (make-pathname :directory '(:relative ".cache" "jscl" "work"))
                   (user-homedir-pathname)))

(ensure-directories-exist *work-dir*)

(defun mesh.js-pathname ()
  (make-pathname :name "mesh" :type "js" :version :newest
                 :directory (pathname-directory *work-dir*)))

(defun sort-sources-package-first (a b)
  (cond
    ((equal (pathname-name a) "package") t)
    ((equal (pathname-name b) "package") nil)
    (t (string-lessp (pathname-name a) (pathname-name b)))))

(defun mesh.js-source-pathnames ()
  (sort
   (directory (make-pathname :name :wild :type "lisp"
                             :version :newest
                             :directory (pathname-directory *mesh-dir*)))
   #'sort-sources-package-first))

(defun js-needs-rebuild-p ()
  (dolist (lisp (mesh.js-source-pathnames))
    (let ((js (mesh.js-pathname)))
      (when (or (not (probe-file js))
                (< (file-write-date js) (file-write-date lisp)))
        (return-from js-needs-rebuild-p t)))))

(defun rebuild-js ()
  (format *terminal-io* "~&Rebuilding Javascript (JSCL compilation)…")
  (ensure-directories-exist (mesh.js-pathname))
  (tagbody
   do-over
     (restart-case
         (uiop/stream:with-temporary-file (:stream skinny-js
                                                   :pathname skinny-js-pathname
                                                   :directory *work-dir*)
           (load (make-pathname :name "package" :type "lisp" :version :newest
                                :directory (pathname-directory *mesh-dir*)))
           (write-string (uglify (with-output-to-string (fat-js)
                                   (jscl:write-javascript-for-files
                                    (mesh.js-source-pathnames) fat-js)))
                         skinny-js)
           (uiop/filesystem:rename-file-overwriting-target skinny-js-pathname
                                                           (mesh.js-pathname)))
       (do-over ()
         :report "Try the JSCL cross-compilation again"
         (go do-over)))))

(defun jscl-rebuild-if-needed ()
  (when (js-needs-rebuild-p)
    (rebuild-js)))

(defun jscl-rebuild-when-needed ()
  (inotify:with-inotify
      (inotify
       `((,*mesh-dir* ,(logior inotify:in-close-write inotify:in-moved-to))))
    (loop for event = (inotify:read-events inotify)
       thereis event
       do (handler-case 
              (jscl-rebuild-if-needed)
            (error (c)
              (warn "Got error ~s (~:*~a), trying again in 10s…" c)
              (sleep 10)
              (invoke-restart 'do-over))))))

(defun join-lines (a b)
  (concatenate 'string a #(#\Newline) b))

(defparameter *uglifyp* nil)
(defparameter *prettyp* t)

(defun uglify-with-parameters (js)
  (if (or *uglifyp*
          *prettyp*)
      (cl-uglify-js:ast-gen-code
       (funcall
        (if *uglifyp*
            #'cl-uglify-js:ast-mangle
            #'identity)
        (cl-uglify-js:ast-squeeze
         (parse-js:parse-js js :ecma-version 5)
         :sequences t :dead-code t))
       :beautify *prettyp*)
      js))

(defun uglify (js)
  (let ((js (etypecase js
              (stream (read-file-into-string js :external-format :utf-8))
              (string js))))
    (format *terminal-io* "~&Uglifying: source is ~:d chars;" (length js))
    (let ((outcome (uglify-with-parameters js)))
      (progn
        (format *terminal-io* "~&Uglifying: outcome is ~:d chars;" (length outcome))
        outcome))))

(defun print-error-as-js-comment (c source-path)
  (format nil "

/* —————————— */
console.log(\"Compile-time error: ~a in …>jscl>~a\");
/*
~a
 * —————————— */

"
          (type-of c) (pathname-name source-path) c))

(defun jscl-find-or-compile (js-path)
  (response-is-js)
  (jscl-compile-js<-lisp* js-path)
  (uglify (read-file-into-string js-path)))

(defmacro with-jscl-padding ((source-path) &body body)
  `(reduce #'join-lines
           (list
            "if(window.jscl===undefined){
 alert(\"Common Lisp support missing from Javascript environment.\");
}"
            (format nil "console.log(\"Loading ~a.lisp…\");"
                    (pathname-name ,source-path))
            "(function(values, internals){"
            ,@body
            "})(window.jscl.internals.pv, window.jscl.internals);"
            (format nil "console.log(\"…Loaded ~a.lisp.\");"
                    (pathname-name ,source-path)))))

(defun jscl-compile-js<-lisp* (js-path)
  (handler-bind
      ((sb-kernel::simple-package-error
        (lambda (c)
          (declare (ignore c))
          (continue))))
    (let ((jscl::*compile-print-toplevels* t)
          (*package* (find-package :tootstest.js)))
      (jscl::compile-application
       (list (make-pathname :type "lisp" :defaults js-path))
       js-path)))
  (unless (probe-file js-path)
    (error "Compilation did not produce ~a" js-path)))

(declaim (special jscl::*environment*))

(defun jscl-compile-js<-lisp (js-path)
  (with-output-to-file (out js-path :if-exists :supersede)
    (let ((jscl::*environment* (jscl::make-lexenv))
          (source-path (make-pathname :type "lisp" :defaults js-path))
          (*features* (cons :jscl *features*))
          (*package* (find-package "TOOTSTEST.JS")))
      (write-string (with-jscl-padding (source-path)
                      (with-output-to-string (s)
                        (jscl::with-compilation-environment
                            (jscl::!compile-file source-path s))))
                    out))))

(defun jscl-compile-file (source-path)
  (let ((jscl::*environment* (jscl::make-lexenv))
        (*features* (cons :jscl *features*)))
    (with-jscl-padding (source-path)
      (jscl::with-compilation-environment
        (handler-case
            (with-output-to-string (s)
              (let ((*package* (find-package :jscl)))
                (with-input-from-file (in source-path)
                  (loop with eof = (gensym "EOF")
                     for form = (handler-case
                                    (jscl::ls-read in nil eof)
                                  (error (c)
                                    (format nil "/* error in READ: ~a */" c)
                                    '()))
                     until (eql eof form)
                     collect (format nil "/* form (~s ~s … ) */"
                                     (first form)
                                     (second form))
                     collect (jscl::js form))))))))))

(defun response-is-js ()
  (appendf (response-headers *response*)
           (list "Content-Type"
                 "application/javascript;charset=utf8")))

(defun render-script (source-path &optional env)
  (declare (ignore env))
  (response-is-js)
  (reduce #'join-lines
          (list "/* -*- js2 -*- */"
                (concatenate 'string "/* JSCL ← "
                             (pathname-name source-path) " */")
                "'use strict';"
                (uglify (jscl-compile-file source-path))
                "/* © 2016 */")))

(defroute "/tootstest/jscl/:file-name.lisp" (&key file-name)
  (render-script (merge-pathnames
                  (make-pathname :directory '(:relative "jscl")
                                 :name file-name
                                 :type "lisp")
                  *template-directory*)))

(defroute "/tootstest/jscl/:file-name.js" (&key file-name)
  (jscl-find-or-compile (merge-pathnames
                         (make-pathname :directory '(:relative "jscl")
                                        :name file-name
                                        :type "js")
                         *template-directory*)))

(defroute "/tootstest/gossipnet/mesh.js" ()
  (block mesh.js
    (handler-case
        (with-simple-restart (continue "Try again")
          (let ((s (read-file-into-string (mesh.js-pathname))))
            (appendf (response-headers *response*)
                     `(:Content-Type "application/javascript; charset=utf-8"
                                     :Content-Length ,(length s)))
            s))
      (sb-int:simple-file-error (c)
        (declare (ignore c))
        (rebuild-js-if-needed)
        (continue)))))


(defun jscl-rebuild-thread ()
  (tootstest.web:jscl-rebuild-if-needed)
  (loop (handler-case
            (tootstest.web:jscl-rebuild-when-needed)
          (error (c)
            (warn "JSCL error condition: ~a" c) t))))
