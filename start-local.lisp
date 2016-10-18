;;; -*- lisp  -*- LOAD this  file to start  a local loopback  server for
;;; testing

(format t "~|~3%TOOTSTEST local start

Grabbing Tootstest ASDF…")
(asdf:load-asd (merge-pathnames #p"./tootstest.asd" *load-pathname*))
(format t "~&~|~%Loading system…")
(ql:quickload :tootstest)
(format t "~&~|~%Starting local server…")
(eval (read-from-string "(tootstest:start)"))
(format t "~2&Opening in browser…")
(uiop:run-program "xdg-open http://localhost:5000/tootstest/")
(format t "~2&Ready.")
