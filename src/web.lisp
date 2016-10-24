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


;;; Default route

(defroute "/" ()
  "This is encountered only in local testing."
  (redirect-to "/tootstest/"))

(defroute "/tootstest/" ()
  (redirect-to "/tootstest/login"))


;;; Static assets

(defun send-static (content-type pathname)
  (appendf (response-headers *response*)
           (list "Content-Type" content-type))
  (read-file-into-string pathname))

(defroute "/tootstest/css/:name.css" (&key name)
  (send-static "text/css;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "css")
                                 :name name :type "css")
                                *static-directory*)))

(defroute "/tootstest/js/:name.js" (&key name)
  (send-static "application/javascript;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "js")
                                 :name name :type "js")
                                *static-directory*)))


(defroute "/tootstest/login" ()
  (render #P"login.html"))

;; News

(defvar *tootsbook-cache* nil)
(defvar *tootsbook-fetched* 0)

(defvar *tootsbook-refresh-seconds* (* 60 5))

(defun fetch-tootsbook/http ()
  (setf *tootsbook-cache*
        (cxml:parse-octets
         (drakma:http-request
          (puri:uri "http://www.tootsbook.com/tootsbook/feed/")
          :content-type "application/rdfxml")
         (cxml-dom:make-dom-builder))))

(defun tootsbook-headlines ()
  (when (> (get-universal-time)
           (+ *tootsbook-refresh-seconds* *tootsbook-fetched*))
    (fetch-tootsbook/http))
  *tootsbook-cache*)

(defun rdf-story-to-plist (story)
  (loop for (tag label)
     in '(("title" :title) ("link" :link)
          ("content:encoded" :content) ("description" :description))
     collect label
     collect (get-text-of-element story tag)))

(defun tootsbook-headline-stories ()
  (dom:get-elements-by-tag-name
   (dom:document-element
    (tootsbook-headlines)) "item"))

(defun unescape-& (string)
  (cl-ppcre:regex-replace-all "&amp;" string "&"))

(defun get-text-of-element (node element)
  (apply #'concatenate 'string
         (map 'list (compose #'unescape-& #'dom:node-value)
              (dom:child-nodes
               (first-elt
                (dom:get-elements-by-tag-name node element))))))

(defun tootsbook-news-plists ()
  (map 'list #'rdf-story-to-plist
       (tootsbook-headline-stories)))

(defroute "/tootstest/news" ()
  (render #p"news.html"
          (list :headlines (tootsbook-news-plists))))


;;; ZOMG

(define-constant +application/json+ "application/json"
  :test 'equal)

(defroute "/tootstest/zomg" ()
  (assert (string-equal +application/json+ (request-content-type *request*)
                        :end2 #.(length +application/json+))
          () "Zombies should always use JSON, not ~a" (request-content-type *request*)) 
  (let ((report (let ((yason:*parse-object-as* :plist)
                      (yason:*PARSE-json-arrays-as-vectors* t))
                  (yason:parse (request-raw-body *request*)))))
    (format *error-output* "ZOMG! error report from client:~%~s" report)))

(defroute "/tootstest/version" ()
  (render #p"version.html"
          (list :product "tootstest"
                :version (asdf:component-version (asdf:find-system :tootstest))
                :machine (list :version (machine-version)
                               :type (machine-type)
                               :instance (machine-instance))
                :lisp (list :type (lisp-implementation-type)
                            :version (lisp-implementation-version))
                :software (list :type (software-type)
                                :version (software-version))
                :acceptor (list :name (request-server-name *request*)
                                :port (request-server-port *request*)
                                :protocol (request-server-protocol *request*))
                :copyright-latest #.(local-time:format-timestring
                                     nil (local-time:now)
                                     :format '(:year))
                :build-date #.(local-time:format-timestring
                               nil (local-time:now)
                               :format '(:year #\- :month #\- :day)))))

;; Error pages

(defun wants-json-p ()
  (search "application/json" (gethash "Accept" (request-headers *request*))))

(defmethod on-exception ((app <web>) code)
  (declare (ignore app))
  (cond
    ((wants-json-p)
     (render-json `((:error . ,code))))
    (t (redirect-to (format nil "https://www.tootsville.org/error/~d.shtml" code)))))
