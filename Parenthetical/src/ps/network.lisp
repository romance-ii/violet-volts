;; -*-  mode: lisp; compile-command: make;  -*-

(console-log (concat "


     ***** Violet Volts

     ***** Game client test/demo

Starting Violet Volts: Version " (@ document 'version)))

(defvar *rate-test* (= -1 ((@ window location hash index-of) 'nogoat)))

(defvar *start-time* (+ (new *date)))

(defvar *game-state*
  (create 'logged-in         false
          'player-name       "FrameTestUser"
          'save-auth-token   t
          'sidekick-bound    false))

(setf (@ document 'loaders) (create))

(defun log-in ()
  (when (and (@ navigator probably-bad)
              (y-or-n :browser-bad-get-ffox))
         ((@ window open) "http://getfirefox.com/"))
  (when (@ *game-state* logged-in) (return-from log-in false))

  (setf (@ *game-state* 'player-name)
        (prompt :login-player-char-name (@ *game-state* 'player-name)))
  (remember 'player-name (@ *game-state* 'player-name))
  (if (< 2 (length (@ *game-state* player-name)))
      (progn (setf (@ *game-state* 'logged-in) t)
             (setf (@ (by-id 'log-in) 'style 'visibility) 'hidden)
             (setf (@ (by-id 'add-sidekick) 'style 'visibility) 'visible)
             (setf (@ (by-id 'log-out) 'style 'visibility) 'visible)
             (console-log (concat "Logged in as " (@ *game-state* 'player-name)))
             (start-render))
      (report "OK, not logging in then.")))

(defun add-sidekick () (report "Sorry, you can't have a sidekick help you today."))
(defun drop-sidekick () (report "Right. YOU have a SIDEKICK?"))

(defun log-out ()
  (if (@ *game-state* 'logged-in)
      (progn (setf (@ *game-state* 'logged-in) false)
             (setf (@ (by-id 'log-in) 'style 'visibility) 'visible)
             (setf (@ (by-id 'add-sidekick) 'style 'visibility) 'hidden)
             (setf (@ (by-id 'log-out) 'style 'visibility) 'hidden)
             (report :bye-bye)
             ((@ window location reload) t)
             t)
      (progn (report "You're not logged in.")
             f)))

(setf (@ *game-state* 'player-name) (recall 'player-name ""))

(console-log "Networking ready")




