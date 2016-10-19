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
  (hunchentoot:redirect "/tootstest/login"))

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

(defun get-text-of-element (node element)
  (apply #'concatenate 'string
         (map 'list #'dom:node-value
              (dom:child-nodes
               (first-elt
                (dom:get-elements-by-tag-name node element))))))

(defun tootsbook-news-plists ()
  (map 'list #'rdf-story-to-plist
       (tootsbook-headline-stories)))

(defroute "/tootstest/news" ()
  (render #p"news.html"
          (list :headlines (tootsbook-news-plists))))

;; Error pages

(defun wants-json-p ()
  (search "application/json" (gethash "Accept" (request-headers *request*))))

(defmethod on-exception ((app <web>) code)
  (declare (ignore app))
  (cond
    ((wants-json-p)
     (render-json `((:error . ,code))))
    (t (hunchentoot:redirect (format nil "https://www.tootsville.org/error/~d.shtml" code)))))
