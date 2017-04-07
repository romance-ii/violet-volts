(in-package :tootstest.web)

(defvar *gossip-users* nil)

(defclass user ()
  ((google-token :type string :accessor user-google-token :initarg :google-token)
   (facebook-token :type string :accessor user-facebook-token :initarg :facebook-token)
   (id :type fixnum :accessor user-id :initarg :id)
   (remote-address :accessor user-remote-address :initarg :remote-address)
   (sdp-offer :accessor user-sdp-offer :initarg :sdp-offer :initform nil)
   (sdp-answer :accessor user-sdp-answer :initarg :sdp-answer :initform nil)))

(defun make-user (&rest args)
  (apply #'make-instance 'user args))

(defun user= (a b &rest rest)
  (and (equal (user-id a) (user-id b))
       (if rest (apply #'user= a rest)
           t)))

(defun find-user-by-sdp (sdp)
  (when-let (found (remove-if-not (lambda (user)
                                    (equal sdp (user-sdp-offer user)))
                                  *gossip-users*))
    (first found)))

(defun find-user-by-google-token (id-token)
  (let ((token (handler-case
                   (user<-google-token id-token)
                 (simple-error (c)
                   (declare (ignore c)) 
                   (when (member (request-server-name *request*) '("localhost" "::1" "127.0.0.1") :test 'equal)
                     (warn "Falling back on special case for localhost testing")
                     (format nil "~a:~d" (subseq id-token 0 (position #\. id-token)) (request-remote-port *request*)))))))
    (if-let (found (remove-if-not (lambda (user)
                                    (equal token (user-google-token user)))
                                  *gossip-users*))
      (first found)
      (with-connection (db :members)
        (or (retrieve-one (select :*
                            (from :users)
                            (where (:= :google-token token)))
                          :as 'user)
            (progn
              (execute (insert-into :users
                         (set= :google-token token)))
              (retrieve-one (select :*
                              (from :users)
                              (where (:= :id (:last_insert_id))))
                            :as 'user)))))))

(defun request-param-value (param)
  (when-let (found (assoc
                    param
                    (request-body-parameters *request*)
                    :test 'equal))
    (cdr found)))

(defun gossipnet-update-client (user)
  (setf (user-remote-address user) (request-remote-addr *request*))
  (when-let (sdp (request-param-value "sdp"))
    (when (not (equal sdp (user-sdp-offer user)))
      (setf (user-sdp-answer user) nil))
    (setf (user-sdp-offer user) sdp))
  (pushnew user *gossip-users* :test #'user=))

(defun find-user-from-session ()
  (when-let ((google-token (request-param-value "google-api-token")))
    (find-user-by-google-token google-token)))

(defun active-sdp-offers (user)
  (map 'list (rcurry #'drakma:url-encode :utf-8)
       (map 'list #'user-sdp-offer 
            (remove-if #'user-sdp-answer
                       (remove-if-not #'user-sdp-offer
                                      (remove-if (curry #'user= user)
                                                 *gossip-users*))))))

(defun get-user-info (user)
  (let ((answer (user-sdp-answer user)))
    (gossipnet-update-client user)
    (setf (getf (response-headers *response*) :content-type) "application/json") 
    (let ((offers (active-sdp-offers user)))
      (format nil "{\"id\":~d,~
\"toots\":{\"~:*~d\": \"Toot ~:*~d\"},~
\"nickname\": \"Toot ~:*~d\",~
\"offers\":[~{\"~A\"~^,~}]~
~@[,\"answer\":~a~]}" 
              (user-id user) offers answer))))

(setf (ningle:route *web* "/tootstest/action/gossip/answer"
                    :method :post)
      (lambda (x)
        (declare (ignore x)) 
        (let ((answeror (find-user-from-session))
              (offeror (find-user-by-sdp (request-param-value "offeror"))))
          (cond ((user-sdp-answer offeror)
                 (setf (response-status *response*) 409)
                 "{offeror:\"not-available\"}")
                (t
                 (setf (user-sdp-answer offeror) (request-param-value "answer"))
                 (setf (response-status *response*) 202)
                 "202")))))

(setf (ningle:route *web* "/tootstest/action/gossip"
                    :method :put)
      (lambda (x)
        (declare (ignore x)) 
        (let ((user (find-user-from-session)))
          (unless user
            (error "No user"))
          (setf (response-status *response*) 201)
          (get-user-info user))))

(setf (ningle:route *web* "/tootstest/action/gossip"
                    :method :get)
      (lambda (x)
        (declare (ignore x)) 
        (let ((user (find-user-from-session)))
          (unless user
            (error "No user"))
          (assert (member user *gossip-users* :test #'user=))
          (get-user-info user))))
