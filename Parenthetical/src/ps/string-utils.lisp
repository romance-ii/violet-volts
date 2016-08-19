;; -*- lisp -*-

(defun remember (key value)
  (when (and store (@ store enabled))
    ((@ store set) key value)))

(defun recall (key &optional (default-value nil))
  (when (and store (@ store enabled))
    ((@ store get) key default-value)))

(defvar *message-language* :en)

(defvar *message-catalog*
  (create
   :no-help
   "I wish I could help, but the help you're asking-for hasn't yet
been written. You might need to contact Customer Service."

   :browser-bad-get-ffox
   "Your web browser might not
be able to play this game.

Would you like to download
Firefox first?"
   
   :want-howto-graphics
   "Would you like to learn more about
how to improve the graphics speed
of your computer/device?"

   :login-player-char-name
   "What is your character's name?"   

   :explain-slow-turtle
   "When you see the sad turtle, it means your computer's graphics are
displaying \"slowly.\"

You might see that characters do not move smoothly, or things appear
to be \"jerky.\"

This might happen sometimes when you switch to another program, and
then go away after a minute or so.

If you see it a lot, it could be a problem with your computer
or device."

   :no-webgl-wanna-learn
   "You cannot play this game on this computer (or device) because it
does not have WebGL (3D) graphics capabilities right now.  Would you
like to learn how to enable WebGL?"
   
   :quit-with-sidekick 
   "If you leave, you and your sidekick will leave Violet Volts.
Are you sure?"

   :quit-without-sidekick
   "If you leave, you will leave Violet Volts.
Are you sure?"
   )

  "The message catalog system is used together with the `msg' function
to store strings that may be user-visible. We support a minimal level
of variable interpolation using the {0} format of String.format in
JavaScript; a polyfill for this method is present in the preamble
JavaScript code.
")

(defun msg (key &rest args)
  (let ((msg-fmt (getprop *message-catalog* key)))
    (if (equal null msg-fmt)
        (let ((s (concatenate 'string "!# MSG: “" key "”")))
          (dolist (arg args)
            (setf s (concatenate 'string s " (" arg ")")))
          s)
        (apply (@ *string format) msg-fmt args))))


