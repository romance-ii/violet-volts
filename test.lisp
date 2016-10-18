(ql:quickload 'clack :silent t)
(ql:quickload 'lack-middleware-backtrace :silent t)
(ql:quickload 'clack-handler-fcgi :silent t)

(defun main ()
  (clack:clackup
   (lambda (env)
     '(200
       (:content-type "text/plain")
       ("Hello, Clack!")))
   :server :fcgi
   :use-thread nil
   :fd 0))

(sb-ext:save-lisp-and-die "test.fcgi" :toplevel #'main :executable t)
