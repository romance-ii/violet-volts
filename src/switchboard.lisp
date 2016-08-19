(in-package :tootstest.web)

(defclass peer ()
  ((room :accessor peer-room :initarg :room)))

(defclass event-emitter ()
  ((rooms :reader emitter-rooms :initform (make-hash-table :test 'equal))))

(defvar *board* (make-instance 'event-emitter))

(defmethod peer-process ((peer peer) data)
  (let (command parts target)
    (emit board :data data (and peer (id peer)) peer)
    (if (eql (char data 0) #\/)
        (setf command (string-downcase (subseq data 1 (position data #\| )))
              parts (mapcar #'json-parse (split-sequence #\| (subseq data (+ 2 (length command))))))
        (cond
          ((equal command "to")
           (setf target (and (peer-room peer)
                             (remove-if-not (lambda (member)
                                              (equal (member-id member) (first parts)))
                                            (room-members (peer-room peer)))))
           (unless target
             (error "got a request for id ~a but cannot find target" 
                    (first parts)))
           (emit target :data data))
          (t (error "unknown command ~a" command))))))

(defmethod peer-leave ((peer peer))
  (emit *board* :leave peer))

(defun module-exports (opts)
  (let ((board (make-instance 'event-emitter)))
    (defun connect ()
      (make-instance 'event-emitter))))

(defclass room ()
  ((name :initarg :name :type string :reader room-name)
   (members :initform nil :accessor room-members)
   (lock :reader room-lock)))

(defmethod initialize-instance :after ((room room) &key name &allow-other-keys)
  (setf (slot-value room 'lock)
        (make-lock :name (concatenate 'string "Lock around members in " name))))

(defmethod room-members :around ((room room))
  (with-lock-held (room-lock room)
    (call-next-method)))

(defmethod emit ((event-emitter emitter) name source &rest args)
  (unless (eql emitter source)
    (emit emitter args))) ;; wait, what? emitter.emit.apply(emitter, args)

(defun find-room (name)
  (or (gethash name (board-rooms *board*))
      (setf (gethash name (board-rooms *board*)) (make-instance 'room :name name))))

(defmethod add-member-to-room ((peer peer) (room room))
  (with-lock-held (room-lock room)
    (unless (member peer (slot-value room 'members))
      (appendf (slot-value room 'members) peer))))



(defmethod board-announce ((board emitter) payload peer sender data)
  (let ((room-name (and data (getf data :room)))
        (room (and room-name (find-room room-name))))
    (when (and (peer-room peer)
               (not (equal (room-name (peer-room peer)) 
                           room-name)))
      (emit *board* :leave peer sender data))
    (setf (peer-room peer) room
          (peer-id peer) (getf data :id))
    (when room
      (add-member-to-room peer room))
    (emit peer :data (format nil "/roominfo|{\"memberCount\":~d}"
                             (length (room-members room))))))

(defmethod board-leave ((board emitter) peer)
  (when (peer-room peer)
    (remove-member-from-room ((peer peer) (room room)))))

