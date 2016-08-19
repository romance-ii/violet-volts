(console-log "Loading Game Controls…")

(defun ctrl-trap-keys ())

(defun ctrl-trap-mouse ())

(defun ctr-trap-touch ())

(defun ctrl-trap-accel ())

(defun ctrl-ignore-keys ())

(defun ctrl-ignore-mouse ())

(defun ctr-ignore-touch ())

(defun ctrl-ignore-accel ())

(defun trap-game-controls ()
  (ctrl-trap-keys)
  (ctrl-trap-mouse)
  (ctr-trap-touch)
  (ctrl-trap-accel))

(defun ignore-game-controls ()
  (ctrl-ignore-keys)
  (ctrl-ignore-mouse)
  (ctr-ignore-touch)
  (ctrl-ignore-accel))
