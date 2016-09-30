# -*- lisp -*-
(defvar *game-state*
  `( :google-user nil
                  :facebook-user nil
                  :oauth-user nil
                  :location-info nil
                  :player-info nil
                  :overlay-active "game-welcome"
                  :server-info ( :url ,(concatenate 'string
                                                    #j:document:location:protocol
                                                    "//" 
                                                    #j:document:location:host
                                                    "/tootstest/action"))))

(defun logged-in-p () 
  (or (getf *game-state* google-user)
      (getf *game-state* facebook-user)
      (getf *game-state* oauth-user)))

(defun log (format &rest args)
  (#j:console:log (apply #'format nil format args)))

;;; This is called with the results from from FB.getLoginStatus().
(defun facebook-sign-in-changed (response) 
  (log "facebook-sign-in-changed")
  (log response)
  ;; The response object  is returned with a status field  that lets the
  ;;  app know the current login status  of the person. Full docs on the
  ;;  response   object  can   be   found  in   the  documentation   for
  ;;  FB.getLoginStatus().
  (cond ((equal (oget response "status") "connected") 
         (finish-Facebook-Sign-In))              
        ((equal (oget response "status") "not_authorized") 
         (ga "send" "event" "Facebook Sign-In" "Refused" "Refused") ;
         ;; The person is logged into Facebook but not your app.
         (setf (getf *game-state* :facebook-user) nil))
        (t
         ;; The person is not logged into  Facebook so we're not sure if
         ;; they are logged into this app or not.
         (ga "send" "event" "Facebook Sign-In" "Not Signed In" "Not Signed In")
         (setf (getf *game-state* :facebook-user) nil))) 
  (game-status-update))


;; This function is called when someone finishes with the Login
;; Button.  See the onlogin handler attached to it in the sample
;; code below.
(defun check-log-in-state ()
  (#j:FB:getLoginStatus(lambda (response)
                         (facebook-sign-in-changed response))))


(defun signInToFacebook ()
  (#j:FB:login (lambda (response)
                 (facebook-sign-in-changed response))
               (make-object "scope" "public_profile,email")))

(setf #j:window:fbAsyncInit
      (lambda ()
        (#j:FB:init
         (make-object
          "appId" "1265378050154137",
          "cookie" t ; enable cookies to  allow the server to access the
                                        ; session
          "xfbml" true    ; parse social plugins on this page
          "version" "v2.5"   ; use graph api version 2.5
          ))                

        (#j:FB:getLoginStatus
         (lambda (response) (facebook-sign-in-changed response)))))

(defun id-element (id)
  (#j:document:getElementById id))

;; Load the SDK asynchronously
(defun load-facebook-async ()
  (let ((fjs (first (#j:document:getElementsByTagName "script"))))
    (when (id-element "facebook-jssdk")
      (return-from load-facebook-async)))
  (let ((js (#j:document:createElement "script"))) 
    (setf (oget js "id") "facebook-jssdk")
    (setf (oget js "src") "//connect.facebook.net/en_US/sdk.js")
    ((oget* fjs "parentNode" "insertBefore") js fjs)))
(load-facebook-async)

(defun set-html (id html)
  (setf (oget (id-element id) "innerHTML") html))

(defun set-set (id src)
  (setf (oget (id-element id) "src") src))

(defun finish-facebook-sign-in ()
  (#j:FB:api "/me" 
             (make-object "fields" "id,name_format,picture,email") 
             (lambda (response) 
               (cond
                 ((not (oget response "error")) 
                  (let ((name (oget response "name_format")))
                    (log "Successful login for: " name)
                    (log response)
                    (set-html "facebook-name" name)) 
                  (set-src "facebook-photo" (oget* response "picture" "data" "url")) ;
                  (ga "send" "event" "Facebook Sign-In" "Success" "Success"))
                 (t ; error
                  (log response))) 
               (setf (getf game-state :facebook-user) response)
               (game-status-update))))

(defun element-style (id)
  (oget (id-element id) "style"))

(defun show-overlay-layer ()
  (setf (oget (element-style "overlay") "display") "block"))

(defun block/none (flag)
  (if flag "block" "none"))

(defun element-display-block-p (id)
  (equal "block" (oget (element-style id) "display")))

(defun set-element-display-block (id blockp)
  (setf (oget (element-style id) "display") (block/none flag)))

(defun hide-login-overlay ()
  (set-element-display-block "login-overlay" nil))

(defun show-login-overlay ()
  (ga "send" "screenview" (make-object "screenName" "game-welcome"))
  (set-element-display-block "login-overlay" t)
  (set-element-display-block "game-welcome" nil))

(defun sign-in-layers-toggle (service signed-in-p)
  (set-element-display-block (concatenate 'string service "-signed-in")
                             (block/none signed-in-p))
  (set-element-display-block (concatenate 'string service "-not-signed-in")
                             (block/none (not signed-in-p))))

(defun show-active-overlay (overlay-active)
  (log (concatenate 'string "active overlay: " overlay-active))
  (when (not (element-style-display-block-p overlay-active))
    (ga "send" "screenview" (make-object "screenName" overlay-active)))
  (set-element-display-block overlay-active t))

(defun game-status-update ()
  (log "game-status-update")
  (cond 
    ((or (not (logged-in-p))
         (getf game-state :overlay-active)) 
     (show-overlay-layer) 
     (when (not (logged-in-p))
       (log "user not signed in"))
     (when (not (element-display-block-p "login-overlay"))
       (show-login-overlay)))
    (t (hide-login-overlay)))

  (sign-in-layers-toggle "google" (getf *game-state* :google-user))
  (sign-in-layers-toggle "facebook" (getf *game-state* :facebook-user))
  
  (unless (getf *game-state* :player-info)
    (log "waiting for game server reply")
    (return-from game-status-update))
  (let ((overlay-active (getf *game-state* :overlay-active)))
    (cond (overlay-active 
           (show-active-overlay overlay-active)           )
          (t (log "game play mode active")))))

(defun google-sign-in-failed ()
  (#j:alert "Google sign-in failed. Please try again.")
  (ga "send" "event" "Google Sign-In" "Failed" "Failed")
  (game-status-update))

(defun google-sign-in-button ()
  (log "Google API loaded. Need a sign-in button?")
  (game-status-update)
  (let ((auth (#j:gapi:auth2:getAuthInstance)))
    (cond (((oget auth "isSignedIn"))
           (log "Guess not! Let's fix the game state.")
           (got-google-sign-in (oget auth "currentUser" "get")))
          (t (log "Draw the sign-in button.")
             (return-from google-sign-in-button
               (#j:gapi:signin2:render "my-signin2" 
                                       (make-object
                                        "scope" "profile email"
                                        ;; "width": 240,
                                        ;; "height": 50,
                                        "longtitle" true
                                        "theme" "dark"
                                        "onsuccess" got-google-sign-in
                                        "onfailure" google-sign-in-failed)))))))

(defun make-new-player ()
  (let ((nickname "Bubba")) 
    (ga "send" "event" "Game Sign-In" "Make New Player" nickname)
    (list :nickname ,nickname
          :hit-points 10
          :max-hit-points 10
          :inventory '((:name "Blue Jeans"
                        :id #x1000
                        :equip "pants"
                        :equipped-p t
                        :durability 1
                        :repair 1
                        :max-repair 1 )))))

(defun got-google-sign-in (google-user)
  (setf (getf *game-state* :google-user) google-user)
  (let ((profile ((oget google-user "getBasicProfile")))) 
    (log (concatenate 'string "Google signed in with " 
                      (funcall (oget profile "getEmail"))))
    (ga "send" "event" "Google Sign-In" "Success" "Success")
    (set-html "welcome-name" (funcall (oget profile "getName")))
    (set-src "welcome-photo" (funcall (oget profile "getImageUrl")))
    (setf (getf *game-state* :player-info) (make-new-player)) 
    (game-status-update)))

(defun signInToMesh (response)
  (log "Sign in to game mesh now; received directory")
  (when (response) (log response))
  (ga "send","event","Game Sign-In" "Join Mesh" "Join Mesh")
  ;; TODO: use returned figures from join-gossip call 
  (let ((rando (floor (random 10000))))
    (set (getf *game-state* :player-info)
         (make-object "id" rando "nickname" (concatenate 'string "Testing " rando)))
    (game-status-update)))

(defun network-error (condition)
  (#j:alert "A network error occurred. Is the network disconnected?"))

(defun xhr-report-errors (xhr)
  (setf (oget xhr "onerror")
        (lambda (condition)
          (network-error condition))))

(defun server-url (suffix)
  (concatenate 'string (getf (getf *game-state* :server-info) :url) suffix))

(defun xhr-set-up-post (xhr uri on-load)
  ((oget xhr "open") "PUT" (server-url uri))
  ((oget xhr "setRequestHeader") "Accept" "application/json")
  ((oget xhr "setRequestHeader") "Content-Type" "application/x-www-form-urlencoded")
  (setf (oget xhr "onload") on-load)
  (xhr-report-errors xhr))

(defun google-api-token ()
  (oget (funcall (oget (getf *game-state* :google-user)
                       "getAuthResponse"))
        "id_token"))

(defun sign-in-to-game ()
  (log "Sign in to game mesh now; get directory")
  (let ((xhr (make-new 'XMLHttpRequest))) 
    (ga "send" "event" "Game Sign-In" "Start" "Start")
    (xhr-set-up-post xhr "/gossip" (lambda (response) 
                                     (sign-In-To-Mesh response)))
    ((oget xhr "send") (concatenate 'string "google-api-token="
                                    (google-api-token)))))

(defun quit-from-game ()
  (let ((xhr (make-new 'XMLHttpRequest)))
    (ga "send" "event" "Game Sign-In" "Quit" "Quit")
    (xhr-set-up-post xhr "/quit"
                     (lambda ()
                       (setf (getf *game-state* :player-info) nil)))
    (game-status-update)
    (funcall (oget xhr "send"))))

(defmacro -then-> (first &rest then)
  `(funcall (oget (funcall ,first) "then") (lambda () ,then)))

(defun quit-from-google ()
  (ga "send" "event" "Google Sign-In" "Sign Out" "Sign Out")
  (let ((auth2 (#j:gapi:auth2:getAuthInstance)))
    (-then-> (oget auth2 "signOut")
             (setf (getf *game-state* :google-user) nil) 
             (game-status-update))))

(defun quit-from-oauth ()) 
(defun quit-from-facebook () 
  (#j:FB:logout)) 

(defun quit ()
  (when (not (or ((getf *game-state* :player-info) 
                  (logged-in-p)))) 
    (game-status-update))
  (when (gameState.playerInfo) (quit-from-game))
  (when (gameState.googleUser) (quit-from-google))
  (when (gameState.facebookUser) (quit-from-facebook))
  (when (gameState.oauthUser) (quit-from-oauth)))
