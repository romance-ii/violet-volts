;; -*- lisp -*-
(load "~/quicklisp/setup")
(ql:quickload :parenscript)
(ql:quickload :trivial-backtrace)

(format t "~%~%~% Parenscript: Compiling ~%~%~%")

(in-package :parenscript)

(format t "~% Loading macros…")
(load "src/ps/00-macros.lisp")

(defun dump-trace ()
  (loop for frame in (sb-debug:list-backtrace)
     for fun = (car frame)
     when (and (symbolp fun)
               (equal (symbol-package fun)
                      (symbol-package 'ps:ps)))
     do (format *error-output* "
________________________________________________________________________
~:(~A~)~{~&~T~S~}" fun (rest frame))))




(format t "~% Compiling all Lisp sources in (:relative \"src\" \"ps\")")


(loop for src-file in (directory
                       (make-pathname
                        :directory (list :relative "src" "ps")
                        :name :wild
                        :type "lisp"))
   for src-file-name = (pathname-name src-file)
   unless (or (char= (elt src-file-name 0) #\.)
              (string-equal "00-macros" src-file-name))
   do
     (with-open-file (source src-file :direction :input)
       (format t "~&~% Source file: ~S" src-file)
       (format t "~&~A:0" (truename src-file))
       (let ((out-file (make-pathname :directory
                                      (list :relative "build")
                                      :name src-file-name
                                      :type "js")))
         (with-open-file (js-out
                          out-file
                          :direction :output
                          :if-exists :rename-and-delete)
           (format t "~& Compiling to: ~S ~%~%" out-file)
           (loop
              for form = (read source nil :the-end)
              for form-count from 1
              until (eql :the-end form)
              do (progn
                   (format t "~& • ~A: form ~:D (~A ~A)" src-file-name form-count
                           (if (symbolp (car form)) (car form) "…")
                           (if (symbolp (second form)) (second form) "…"))
                   (format js-out "~%~%/* ~S form #~:D~%~S~%*/~%" src-file form-count form)
                   (handler-bind
                       ((parenscript::compile-expression-error
                         (lambda (e)
                           (format *error-output* "
************************************************************************
Error in Parenscript: Cannot compile form to an expression

The form probably cannot be used here, because of JavaScript's
dichotomy of expressions v. statements. You'll need to move the
expression out of the containing form somehow.

 • Source file: ~S
 • Form: ~S
 • Top-level form: ~S
" src-file (parenscript::error-form e) form)
                           (dump-trace)
                           (invoke-restart 'continue)))


                        ((or simple-error type-error)
                         (lambda (e)
                           (format *error-output* "
************************************************************************
Error in Parenscript: ")
                           (trivial-backtrace:print-condition e *error-output*)
                           (format *error-output* "~%~%
Compilation of this file has failed…~%")
                           (format js-out "~%~%/*~%   ")
                           (dolist (out (list *error-output* js-out))
                             (format out "Compilation failed:
 • Source file: ~S
 • Failure condition:~%" src-file)
                             (trivial-backtrace:print-condition e out)
                             (format out "~% • Form: ~S~%" form))
                           (format js-out "~% */~%")
                           (invoke-restart 'abort))))
                     (princ (eval `(ps:ps ,form)) js-out))))))))

(format t "~%~% Done compiling all Parenscript modules.~%~%")

(common-lisp-user::quit)

