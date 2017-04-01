(in-package :tootstest.web)

(defvar *gossip-users* nil)

(defclass user ()
  ((google-token :type string :accessor user-google-token :initarg :google-token)
   (id :type fixnum :accessor user-id :initarg :id)
   (remote-address :accessor user-remote-address :initarg :remote-address)
   (sdp-offer :accessor user-sdp-offer :initarg :sdp-offer)))

(defun user= (a b &rest rest)
  (and (equal (user-id a) (user-id b))
       (if rest (apply #'user= a rest)
           t)))

(defun find-user-by-google-token (token)
  (if-let (found (remove-if-not (lambda (user)
                                  (equal token (user-google-token user)))
                                *gossip-users*))
    (first found)
    (with-connection (db)
      (or (retrieve-one (select :*
                          (from :users)
                          (where (:= :google-token token))
                          :as 'user))
          (progn
            (execute (insert-into :users
                       (set= :google-token token)))
            (find-user-by-google-token token))))))

(defun request-param-value (param)
  (when-let (found (assoc
                    param
                    (request-body-parameters *request*)
                    :test 'equal))
    (cdr found)))

(defun gossipnet-update-client (user)
  (setf (user-remote-address user) (request-remote-addr  *request*))
  (when-let (sdp (request-param-value "sdp"))
    (setf (user-sdp-offer user) sdp))
  (pushnew user *gossip-users* :test #'user=))

(defun find-user-from-session ()
  (when-let ((google-token (request-param-value "google-api-token")))
    (find-user-by-google-token google-token)))

(setf (ningle:route *web* "/tootstest/action/gossip"
                    :method :put)
      (lambda (x)
        (declare (ignore x))
        (warn "Starting PUT to gossipnet listing")
        (let ((user (find-user-from-session)))
          (unless user
            (error "No user"))
          (gossipnet-update-client user)
          (format nil "User-Record:~2%~s~2%Request:~2%~s" user *request*))))

(setf (ningle:route *web* "/tootstest/action/gossip"
                    :method :get)
      (lambda (x)
        (declare (ignore x))
        (warn "GET on gossipnet")
        (let ((user (find-user-from-session)))
          (unless user
            (error "No user"))
          (assert (member user *gossip-users* :test #'user=))
          (gossipnet-update-client user)
          (format nil "User-Record:~2%~s~2%Request:~2%~s" user *request*))))
