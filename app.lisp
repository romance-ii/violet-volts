(ql:quickload :tootstest)

(defpackage tootstest.app
  (:documentation "The  tootstest application package for  the login/API
  server process. This is a Caveman2 application.")
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :tootstest.web
                :*web*)
  (:import-from :tootstest.config
                :config
                :productionp
                :*static-directory*))
(in-package :tootstest.app)

(builder
 (:static :path (lambda (path)
                  (if (ppcre:scan "^(?:/image/|/css/|/js/|/robot\\.txt$|/favicon\\....$)" path)
                      path
                      nil))
          :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session (if (productionp)
              nil
              (lambda (app)
                (lambda (env)
                  (let ((datafly:*trace-sql* t))
                    (funcall app env)))))
 *web*)
