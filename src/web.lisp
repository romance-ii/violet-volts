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

(defun redirect-to (uri &optional (status 307))
  "Redirect to another URI. Status code 307 for temporary, 301 or 308 for permanent (typically)."
  (list
   status
   `(:location ,uri
               :x-redirected-by ,(asdf:component-version (asdf:find-system :tootstest))
               :content-type "text/html")
   (flexi-streams:string-to-octets
     (concatenate 'string
                  "<!DOCTYPE html><html><title>Redirect</title><a href=\""
                  uri
                  "\">Redirected to "
                  uri
                  "</a></html>"))))


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

;;; Login

(defroute "/tootstest/login" ()
  (render #P"login.html"))

(defvar *google-public-key*)

(defun fetch-json (uri)
  (yason:parse
   (map 'string #'code-char
        (drakma:http-request uri :method :get :accept "application/json;charset=utf-8"))
   :object-as :plist
   :object-key-fn (compose #'make-keyword #'string-upcase)
   :json-arrays-as-vectors t))

(defun get-google-public-key ()
  (or *google-public-key*
      (setf *google-public-key*
            (fetch-json (getf (fetch-json "https://accounts.google.com/.well-known/openid-configuration")
                              :jwks_uri)))))

(defun check-jwk (json-web-key header body signature)
  "TODO: Split  id-token into three  parts on #\.  and pass in  with the
public         key        from         Google.        Similar         to
http://ncona.com/2015/02/consuming-a-google-id-token-from-a-server/ "
  :fixme)

(define-constant +google-client-ids+
    '("1052481910502-4tkek5mbv4jf9o16oaclg1v3ugrruilg.apps.googleusercontent.com"
      "1052481910502-sumg845at7g627qdvhhhambm7c7do8e4.apps.googleusercontent.com")
  :test #'equalp)

(defun user<-google-token (id-token)
  (multiple-value-bind
        (response-binary status response-headers response-uri
                         stream stream-closed-p status-reason)
      (drakma:http-request "https://www.googleapis.com/oauth2/v3/tokeninfo"
                           :parameters (list (cons "id_token" id-token))
                           :method :get
                           :accept "application/json;charset=utf-8")
    (declare (ignore stream stream-closed-p))
    (assert (<= 200 status 299) ()
            "While validating Google sign-in token, error ~D: ~A returned by ~A"
            status status-reason response-uri)
    (assert (string= "https://www.googleapis.com/"
                     response-uri :end2 27) ()
                     "While validating Google sign-in token, response returned by ~A, whose authority I do not recognize"
                     response-uri)
    (let* ((response (yason:parse (map 'string #'code-char response-binary)
                                  :object-as :plist
                                  :object-key-fn (compose #'make-keyword #'string-upcase)
                                  :json-arrays-as-vectors t)))
      (assert (find (getf response :iss) '("accounts.google.com" "https://accounts.google.com")
                    :test #'string-equal) ()
                    "While validating Google sign-in token, ISS field returned was ~A, whose authority I do not recognize"
                    (getf response :iss))
      (assert (< (parse-integer (getf response :exp))
                 (local-time:timestamp-to-unix (local-time:now))) ()
                 "The Google sign-in permission has already expired.")
      (getf response :sub))))

(defun render-player-details (registrar opaque-id)
  (let ((player-entity (find-player-entity :registrar registrar opaque-id :self)))
    (render-json `((:sign-in . ((:registrar . ,registrar)
                                (:id ,opaque-id)))))))

(defroute ("/tootstest/login/registrars/:registrar" :method :post)
    (&key registrar id-token)
  (handler-case
      (let ((user (ecase (make-keyword (string-upcase registrar))
                    (:google (user<-google-token id-token))
                    (:facebook (error "Facebook sign-in is not yet supported")))))
        (when user
          (render-player-details :google user))) 
    (error (c)
      (render-json `((:error . (princ-to-string c)))))))


;;; News

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
                               :instance (string-capitalize (machine-instance)))
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
