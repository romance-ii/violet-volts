;;;; news.lisp — latest news feed from Totsbook
(in-package :tootstest.web)

(defvar *tootsbook-cache* nil
  "This is used as a cache for the current headlines from Tootsbook for a short time. See *TOOTSBOOK-REFRESH-SECONDS* for a definition of ``a short time.''")
(defvar *tootsbook-fetched* 0
  "At what  universal-time was  *TOOTSBOOK-CACHE* last fetched  from the
  Tootsbook RDF feed?")

(defvar *tootsbook-refresh-seconds* (* 60 5)
  "How many seconds must pass between checking Tootsbook for new headlines?")

(defun fetch-tootsbook/http ()
  "Fetch new  headlines from  Tootsbook, and store  the RDF-XML  data in
*TOOTSBOOK-CACHE*"
  (let ((rdf (cxml:parse-octets
              (drakma:http-request
               (puri:uri "http://www.tootsbook.com/tootsbook/feed/")
               :content-type "application/rdfxml")
              (cxml-dom:make-dom-builder))))
    (when rdf
      (setf *tootsbook-fetched* (get-universal-time)
            *tootsbook-cache* rdf))))

(defun tootsbook-headlines ()
  "Return  the current  (or,  at least,  fairly  recent) headlines  from
Tootsbook's RDF feed. Uses a local cache, when available.

Returns the RDF as a raw string"
  (when (> (get-universal-time)
           (+ *tootsbook-refresh-seconds* *tootsbook-fetched*))
    (fetch-tootsbook/http))
  *tootsbook-cache*)



(defun rdf-story-to-plist (story)
  "Convert and  RDF story  (DOM object)  into a  property list  with the
title, link, content, and description."
  (loop for (tag label)
     in '(("title" :title) ("link" :link)
          ("content:encoded" :content) ("description" :description))
     collect label
     collect (get-text-of-element story tag)))

(defun tootsbook-headline-stories ()
  "Returns  the  headline  elements   (DOM  objects)  from  the  current
headlines on Tootsbook."
  (dom:get-elements-by-tag-name
   (dom:document-element
    (tootsbook-headlines)) "item"))

(defun unescape-& (string)
  "Replaces SGML-style &amp; with #\&"
  (cl-ppcre:regex-replace-all "&amp;" string "&"))

(defun get-text-of-element (node element)
  "Extracts  the  text  under  the  given ELEMENT  type  under  NODE  as
a singular string."
  (apply #'concatenate 'string
         (map 'list (compose #'unescape-& #'dom:node-value)
              (dom:child-nodes
               (first-elt
                (dom:get-elements-by-tag-name node element))))))

(defun tootsbook-news-plists ()
  "Returns all headlines in Tootsbook currently as a list of property lists, each made by `RDF-STORY-TO-PLIST'."
  (map 'list #'rdf-story-to-plist
       (tootsbook-headline-stories)))

(defroute "/tootstest/news" ()
  "Render the latest news from Tootsbook into the “news” template."
  (render #p"news.html"
          (list :headlines (tootsbook-news-plists))))
