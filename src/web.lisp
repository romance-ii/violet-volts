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

(defun redirect-to (uri &optional (status 307))
  "Redirect to another URI. Status code 307 for temporary, 301 or 308 for permanent (typically).

As a side effect, provides an extremely skeletal HTML redirection page."
  (check-type uri string "A URL string")
  (check-type status (member 301 307 308)
              "An HTTP status code from among 301, 307, or 308")
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
  "This is encountered only in local testing. Redirect to the actual default page."
  (redirect-to "/tootstest/"))

(defroute "/tootstest/" ()
  "Redirect to the login page from the default page for this API version."
  (redirect-to "/tootstest/login"))


;;; Static assets

(defun send-static (content-type pathname)
  "Send a  static file  as a  response to  a query.  This implementation
buffers the  entire file  into RAM, so  it should not  be used  for very
large files. There is currently  no caching whatsoever, either, although
the OS may do some disc caching."
  (check-type content-type string "A MIME content-type, eg, text/html;charset=utf-8")
  (check-type pathname (or string pathname)
              "The pathname ofa local file to be sent.")
  (appendf (response-headers *response*)
           (list "Content-Type" content-type))
  (read-file-into-string pathname))

(defroute "/tootstest/css/:name.css" (&key name)
  "CSS files are served statically."
  (send-static "text/css;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "css")
                                 :name name :type "css")
                                *static-directory*)))

(defroute "/tootstest/js/:name.js" (&key name)
  "JavaScript files are served statically."
  (send-static "application/javascript;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "js")
                                 :name name :type "js")
                                *static-directory*)))

;;; Login

(defroute "/tootstest/login" ()
  "The  login  page   itself  is  hosted  here   to  ensure  same-domain
  interaction with  the API  calls. It's  mostly a  static page,  but we
  render it through the template engine.

This page should have some more precise headers to assist in caching."
  (render #P"login.html"))

(defvar *google-public-key* nil
  "The  public key  for Google's  OpenID signatures.  This is  cached to
 validate the signatures provided by their servers.")

(defun fetch-json (uri)
  "Fetch URI  as an application/json file  and parse it with  Yason into
a property list tree."
  (yason:parse
   (map 'string #'code-char
        (drakma:http-request uri :method :get :accept "application/json;charset=utf-8"))
   :object-as :plist
   :object-key-fn (compose #'make-keyword #'string-upcase)
   :json-arrays-as-vectors t))

(defun get-google-public-key ()
  "Either fetch  the Google  public-key for OpenID,  or, use  the cached
copy in *GOOGLE-PUBLIC-KEY*"
  (or *google-public-key*
      (setf *google-public-key*
            (fetch-json (getf (fetch-json "https://accounts.google.com/.well-known/openid-configuration")
                              :jwks_uri)))))

(defun check-jwk (json-web-key header body signature)
  "TODO: Split  id-token into three  parts on #\.  and pass in  with the
public         key        from         Google.        Similar         to
http://ncona.com/2015/02/consuming-a-google-id-token-from-a-server/ "
  (declare (ignorable json-web-key header body signature))
  (error 'unimplemented))

(define-constant +google-client-ids+
    '("1052481910502-4tkek5mbv4jf9o16oaclg1v3ugrruilg.apps.googleusercontent.com"
      "1052481910502-sumg845at7g627qdvhhhambm7c7do8e4.apps.googleusercontent.com")
  :test #'equalp
  :documentation  "The  set  of  Google  client ID's  for  whom  we  are
 interested in being a proxy for OpenID. These are obtained through the
 Google   Developer   console  and   are   used   to  validate   OpenID
 sign-in requests.")

(defun binary<-uuid (uuid)
  "Return a single (UNSIGNED-BYTE 128) representing UUID"
  (check-type uuid uuid:uuid)
  (let ((binary 0))
    (loop with byte-array = (uuid:uuid-to-byte-array uuid)
       for index from 0 upto 15
       for byte = (aref byte-array index)
       do (setf binary (dpb byte (byte 8 (* 8 index)) binary)))
    binary)
  #+slow-byte-easier-tounderstand
  (loop with bytes = (uuid:uuid-to-byte-array uuid)
     for index from 0 upto 15
     for byte = (aref bytes index)
     for position from 15 downto 0
     for place-value = (expt 2 (* 8 position))
     summing (* byte player-value)))

(defun uuid<-binary (integer)
  "Convert an (UNSIGNED-BYTE 128) into a UUID"
  (check-type integer (unsigned-byte 128))
  (let ((byte-array (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for index from 0 upto 15
       for byte = (ldb (byte 8 (* 8 index)) integer)
       do (setf (aref byte-array index) byte))
    (uuid:byte-array-to-uuid byte-array)))

(assert
 (let ((uuid (uuid:make-v4-uuid)))
   (uuid:uuid= uuid (uuid<-binary (binary<-uuid uuid)))))

(define-condition unimplemented (error)
  ()
  (:documentation "Signals that a feature has not been inmplemented yet"))

(defclass creature-controller ()
  ()
  (:documentation "A base class used for any entity (eg, player) who can
 control a character in the game in some way."))

(defclass player (creature-controller)
  ((player-id :type (unsigned-byte 128) :accessor player-id
              :initform (binary<-uuid (uuid:make-v4-uuid)))
   (player-email :type (or string null) :accessor player-email
                 :initform :email)
   (player-given-name :type (or string null) :accessor player-given-name
                      :initform :given-name)
   (player-surname :type (or string null) :accessor player-surname
                   :initform :surname)
   (player-full-name :type (or string null) :accessor player-full-name
                     :initform :full-name)
   (player-date-of-birth :type (or local-time:date null) :accessor player-date-of-birth
                         :initform :date-of-birth)
   (player-age :type (or (real 0 *) null) :accessor player-age
               :initform :age))
  (:documentation "A class representing a player who is a human being in
 the real world."))

(defun legal-age (date-of-birth)
  (multiple-value-bind (msec sec min hour day month year)
      (local-time:decode-timestamp (local-time:now))
    (declare (ignore msec sec min hour))
    (multiple-value-bind (_1 _2 _3 _4
                             day-of-birth month-of-birth year-of-birth)
        (local-time:decode-timestamp date-of-birth)
      (declare (ignore _1 _2 _3 _4))
      (let ((had-birthday-p (or (< month-of-birth month)
                                (and (= month-of-birth month)
                                     (<= day-of-birth day)))))
        (+ (- year year-of-birth 1)
           (if had-birthday-p 1 0))))))

(defmethod player-age :around (player)
  (let ((dob (player-date-of-birth player)))
    (if dob (legal-age dob) (call-next-method))))

(defun player-adult-p (player)
  (>= (player-age player) 18))

(defun player-under-13-p (player)
  (< (player-age player) 13))

(defun player-over-13-p (player)
  (>= (player-age player) 13))

(defclass item ()
  ()
  (:documentation  "An item  (including a  character or  something) that
 exists in the game world."))

(defclass creature (item)
  ()
  (:documentation  "A creature  who  is moved  in the  game world  under
 some control."))

(defun collect-setters-for-class (class info)
  "GIven a  CLASS name symbol, where  INFO is a property  list suitable for
MAKE-INSTANCE CLASS, collect the values  from the list where the keyword
matches  the first  initarg name  for a  slot, and  the value  following
matches the type specification for it, as well.

Returns an alist where  each car is a writer function  for the slot, and
each cdr, a new value."
  (loop with not-found = 'not-found
     for slot in (closer-mop:class-slots (find-class class))
     for initarg = (first (closer-mop:slot-definition-initargs slot))
     for writer = (or (first (closer-mop:slot-definition-writers slot))
                      (first (closer-mop:slot-definition-writers slot)))
     for type = (or (closer-mop:slot-definition-type slot) t)
     for new-value = (getf info initarg not-found)
     when (not (eql new-value not-found))
     do (assert (typep new-value type) (new-value)
                "Cannot store ~s in slot ~s (type ~s) of class ~s"
                new-value type (closer-mop:slot-definition-name slot)
                type class)
     collect (cons writer new-value)))

(defun update-player-info (player-id info)
  (let ((Δ (collect-setters-for-class 'player info)))
    (when Δ
      (let ((player (find-player-by-id player-id)))
        (loop for (writer . new-value) in Δ
           do (funcall writer player new-value))))))

(defun link-player-to-registration (player registrar id-string)
  "Link a PLAYER object to a login REGISTRAR and their ID-STRING representng that player."
  (check-type player player)
  (check-type registrar (or string symbol) "string-designator for a registrar")
  (check-type id-string string
              "string that uniquely identifies a user across time, distinctive to REGISTRAR")
  (datafly:execute
   (sxql:insert-into :player-registrations
     (sxql:set= :player-id (player-id player)
                :registrar (string registrar)
                :id-string id-string))))

(defun make-user-registration (registrar id-string info)
  "Create  a new  player object  based  upon INFO,  and link  it to  the
REGISTRAR and ID-STRING"
  (check-type registrar (or string symbol) "string-designator for a registrar")
  (check-type id-string string
              "string that uniquely identifies a user across time, distinctive to REGISTRAR")
  (check-type  info  cons
               "property list suitable for MAKE-INSTANCE 'PLAYER")
  (let ((player (apply #'make-instance 'player info)))
    (link-player-to-registration player registrar id-string)))

(defgeneric user-id<-registrar-id (registrar id &optional info)
  (:documentation "Given  a keyword representing  a REGISTRAR and  an ID
that is  unique across time  among users  of that REGISTRAR,  return the
local user  ID for the  player associated  with it. INFO  should contain
only data for which REGISTRAR is a trusted authority; it will be used to
update or intantiate the local records.

Methods on this  generic function specialize upon the  REGISTRAR and may
return   NIL;   in   which   case,  the   :AROUND   method   will   call
MAKE-USER-REGISTRATION  to instantiate  a  record. Otherwise,  methods
should return a unique user ID previously assigned by that function.

The default method should suffice for many types of registrars."))

(defmethod user-id<-registrar-id :around (registrar id &optional info)
  "This  AROUND  method  handles  not-previously-registered  players  by
calling MAKE-USER-REGISTRATION for them."
  (or (let ((player (call-next-method)))
        (when player
          (update-player-info player info))
        player)
      (make-user-registration registrar id info)))

(defun user-id<-registration (registrar id-string)
  (datafly:retrieve-one-value
   (sxql:select (:id)
     (sxql:from :player-registrations)
     (sxql:where (:and (:equal :registrar (string registrar))
                       (:equal :id-string id-string))))))

(defmethod user-id<-registrar-id ((registrar symbol) (id string) &optional info)
  "The default handler  accepts any persistently unique  ID string and
 returns a user object, if one exists."
  (declare (ignore info))
  (user-id<-registration registrar id))

(defun user<-google-token (id-token)
  "Given a token returned from a Google OpeNID sign-in, validate that it
is good, and return the local user object reference associated with it."
  (multiple-value-bind
        (response-binary status response-headers response-uri
                         stream stream-closed-p status-reason)
      (drakma:http-request "https://www.googleapis.com/oauth2/v3/tokeninfo"
                           :parameters (list (cons "id_token" id-token))
                           :method :get
                           :accept "application/json;charset=utf-8")
    (declare (ignore response-headers stream stream-closed-p))
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
      (user-id<-registrar-id :google (getf response :sub)))))

(defun find-player-by-id (id)
  "Retrieve the player object represented by ID."
  (check-type id (unsigned-byte 128))
  (error 'unimplemented))

(defun ensure-player (player)
  "Ensure  that  PLAYER   is  a  PLAYER  object.   Coërces  integers  or
base-36-coded integer strings."
  (etypecase player
    (player player)
    ((integer 1 *) (find-player-by-id player))
    (string (find-player-by-id (parse-integer player :radix 36)))))

(defun render-player-details (player)
  "Return a JSON object describing the player object.

This method needs to be reworked."
  (let ((player (ensure-player player)))
    (render-json `((:player . ((:id .,(player-id player))
                               (:email .,(player-email player))
                               (:given-name .,(player-given-name player))
                               (:surname .,(player-surname player))
                               (:full-name .,(player-full-name player))
                               (:date-of-birth .,(player-date-of-birth player))
                               (:age .,(player-age player))))))))

(defroute ("/tootstest/login/registrars/:registrar" :method :post)
    (&key registrar id-token)
  "Accept a POST request with information about a player sign-in through an
identity provider  identified by the case-insensitive  string REGISTRAR.
An ID-TOKEN unique among all users of REGISTRAR is required.

There is no  need for ID-TOKEN to be distinctive  of a particular player
across time or valid far into  the future. The REGISTRAR will be queried
by a specific method to obtain a reference to a local user.
"
  (handler-case
      (let ((user (ecase (make-keyword (string-upcase registrar))
                    (:google (user<-google-token id-token))
                    (:facebook (error 'unimplemented)))))
        (when user
          (render-player-details user)))
    (error (c)
      (render-json `((:error . (princ-to-string ,c)))))))


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
