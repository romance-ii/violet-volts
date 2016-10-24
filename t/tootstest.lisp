;;; -*- lisp -*- tootstest Unit Tests

(defpackage #:tootstest.test
  (:use :cl :prove))

(in-package #:tootstest.test)

(setf *default-reporter* :fiveam) ; no ANSI color in logs



;;; Unit tests that don't require spinning up any [F]CGI/Hunchentoot server thread[s]

(is (tootstest.web::unescape-& "Foo &amp;#160; Bar")
    "Foo &#160; Bar")




(finalize) ; run tests
