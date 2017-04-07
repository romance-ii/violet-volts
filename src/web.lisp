(in-package :cl-user)
(defpackage tootstest.web
  (:documentation  "This package  contains  the  handlers that  directly
 handle  various URI  routes for  the web  server. These  are generally
 invoked currently via FastCGI, or a running Hunchentoot instance.")
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

;; Application

(defclass <web> (<app>) ()
  (:documentation "This is the local specialization of the Caveman2 application class."))
(defvar *web* (make-instance '<web>)
  "This is the singleton instance of the web application object.")
(clear-routing-rules *web*)



(setf (ningle:requirement *web* :content-type)
      (lambda (content-type)
        (etypecase content-type
          (string (string-equal content-type (request-content-type *request*)))
          (cons (member (request-content-type *request*) content-type
                        :test #'string-equal)))))



(defun wants-json-p ()
  "Does the client request Accept JSON format?"
  (let ((accept (gethash "Accept" (request-headers *request*))))
    (or (search "application/json" accept)
        (search "text/json" accept)
        (search "application/x-json" accept)
        (search ".js" (request-uri *request*)))))

(defun redirect-to/html-body (uri)
  "Returns an octet array that gives a simple redirection link.

This is  a silly  legacy thing  for ancient  browsers that  don't follow
a  3xx   redirection  or  want   to  display  something   while  they're
redirecting. In  real life, it's  rarely encountered by a  real browser,
but sometimes caught by tools like curl or wget with certain settings."
  (flexi-streams:string-to-octets
   (concatenate 'string
                "<!DOCTYPE html><html><title>Redirect</title><a href=\""
                uri
                "\">Redirected to "
                uri
                "</a></html>")))

(defun redirect-to (uri &optional (status 307))
  "Redirect to another URI. Status code 307 for temporary, 301 or 308 for permanent (typically).

As a side effect, provides an extremely skeletal HTML redirection page via `REDIRECT-TO/HTML/BODY'."
  (check-type uri string "A URL string")
  (check-type status (member 301 307 308)
              "An HTTP status code from among 301, 307, or 308")
  (list
   status
   `(:location ,uri
               :x-redirected-by ,(romance-ii-program-name/version)
               :content-type "text/html")
   (redirect-to/html-body uri)))


;;; Default route

(defroute route-/ "/" ()
          "Redirect to the login page from the default page for this API version."
          (redirect-to "/tootstest/login"))



(define-condition unimplemented (error)
  ()
  (:documentation "Signals that a feature has not been inmplemented yet"))

(define-constant +application/json+ "application/json"
  :test 'equal
  :documentation "The string application/json, since we use it so often.")


;; Error pages

(defmethod on-exception ((app <web>) code)
  "Return error with code CODE"
  (declare (ignore app))
  (cond
    ((wants-json-p)
     (render-json `((:error . ,code))))
    (t (redirect-to (format nil "https://www.tootsville.org/error/~d.shtml" code)))))
