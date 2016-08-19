(in-package :cl-user)
(defpackage tootstest.web
  (:use :alexandria
        :cl
        :caveman2
        :tootstest.config
        :tootstest.view
        :tootstest.db
        :datafly
        :sxql)
  (:export #:*web*)
  (:export #:jscl-rebuild-when-needed
           #:jscl-rebuild-thread
           #:jscl-rebuild-if-needed))
(in-package :tootstest.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defun send-static (content-type pathname)
  (appendf (response-headers *response*)
           (list "Content-Type" content-type))
  (read-file-into-string pathname ))

(defroute "/tootstest/" ()
  (render #p"index.html"))

(defroute "/tootstest/js/:name.js" (&key name)
  (send-static "application/javascript;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "js")
                                 :name name :type "js")
                                *static-directory*)))


(defroute "/tootstest/login" ()
  (render #P"login.html"))

;; News

(defroute "/tootstest/news" ()
  (render #p"news.html"))

;; Error pages

(defun wants-json-p ()
  (search "application/json" (gethash "Accept" (request-headers *request*))))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cond
    ((wants-json-p)
     (render-json '((:error . 404))))
    (t (merge-pathnames #P"error/404.html"
                        *template-directory*))))

(defmethod on-exception ((app <web>) code)
  (declare (ignore app))
  (cond
    ((wants-json-p)
     (render-json `((:error . ,code))))
    (t (merge-pathnames #P"error/500.html"
                        *template-directory*))))
