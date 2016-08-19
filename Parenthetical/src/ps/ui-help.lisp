;; -*- lisp -*-

(defun report (&rest key-args)
  (alert (fapply msg key-args)))

(defun y-or-n (&rest key-args)
  (confirm (fapply msg key-args)))

(defun paperdoll ()
  (report "Paperdoll function")
  'TODO)

(defun show-help ()
  (report "Help goes here.")
  'TODO)

(defun explain-slow ()
  (report :explain-slow-turtle)
  (when (y-or-n "Would you like to learn more about
how to improve the graphics speed
of your computer/device?")
    (report :no-help)
    'TODO))

(defmacro key-const (key)
  "Syntactic sugar for the stupid KI_CAPS_CONSTANTS for key names.
Basically just so `CHECK-KEYS' will look nicer."
  (coerce
   (append
    (list #\K #\I #\_)
    (loop for char across (symbol-name key)
       collect (cond
                 ((char= #\- char) #\_)
                 (t (char-upcase char)))))
   'string))

(defmacro keycase (&rest cases)
  "Basically just for `CHECK-KEYS' to be nicer-looking."
  `(let ((keys (new (@ *g-l-g-e -key-input))))
     ,@(loop for (key handler) in cases
          collect
            `(when ((@ keys is-key-pressed) (getprop *g-l-g-e (key-const ,key)))
               (progn ,handler)))))

(defun request-full-screen (enablep)
  (report :request-full-screen))

(defun do-menu ()
  (report :do-menu))

(defun gamepad (delta-x delta-y)
  :TODO)

(defun check-keys ()
  (keycase
   ;; cursor keys
   (up-arrow      (gamepad 0 -1))
   (down-arrow    (gamepad 0 1))
   (left-arrow    (gamepad -1 0))
   (right-arrow   (gamepad 1 0))
   ;; numeric keypad
   (numpad-1      (gamepad -1 1))
   (numpad-2      (gamepad 0 1))   
   (numpad-3      (gamepad 1 1))
   (numpad-4      (gamepad -1 0))
   (numpad-5      (gamepad 0 0))
   (numpad-6      (gamepad 1 0))
   (numpad-7      (gamepad -1 -1))
   (numpad-8      (gamepad 0 -1))
   (numpad-9      (gamepad 1 -1))
   ;; function keys
   (escape (paperdoll))   
   (f1     (show-help))
   (f3     (or (log-in) (log-out)))
   (f10   (do-menu))
   (f11   (request-full-screen :toggle))
   (page-up (request-full-screen t))
   (page-down (request-full-screen f))))

(defun protect-against-unload (evt)
  (defvar message
    (when (@ *game-state* 'logged-in)
      (if (@ *game-state* 'sidekick-bound)
          :quit-with-sidekick
          :quit-without-sidekick)))
  (let ((evt (or evt window.event)))
    (when (and message evt)
      (setf (@ evt return-value) message)))        
  message)

(setf (@ window on-before-unload) protect-against-unload)

(defun open-help (uri)
"Open the help browser to a specific help topic identifier (URI
fragment).

FIXME. The usage should just take a keyword and compute the URI, and
then load it into a Help overlay layer, but this will work for now…"
  (setf (@ window location) uri))


