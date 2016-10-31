(in-package :tootstest.db)

;;; UUID  interation with  the database.  We handle  them internally  as
;;; (UNSIGNED-BYTE 128), and into MariaDB  as a BINARY(16), but may want
;;; to also sometimes use UUID objects directly (eg, to generate them).

(defun binary<-uuid (uuid)
  "Return a single (UNSIGNED-BYTE 128) representing UUID"
  (check-type uuid uuid:uuid)
  (let ((binary 0))
    (loop with byte-array = (uuid:uuid-to-byte-array uuid)
       for index from 0 upto 15
       for byte = (aref byte-array index)
       do (setf binary (dpb byte (byte 8 (* 8 index)) binary)))
    binary))

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


;;; Items, Creatures

(defclass item ()
  ()
  (:documentation  "An item  (including a  character or  something) that
 exists in the game world."))

(defclass creature (item)
  ()
  (:documentation  "A creature  who  is moved  in the  game world  under
 some control."))


;;; Creatures, Players.

;; (defmodel      creature-controller       (:inflate      registered-at
;; #'datetime-to-timestamp) id registered-at)

;; (defclass  creature-controller  ()   ((creature  :type  (or  creature
;;   (unsigned-byte            128)           null)            :accessor
;;   creature-controller-creature :initarg  :creature :documentation "If
;;   this  controller  is  currently  controlling a  creature,  this  is
;;   a  reference  to that  creature;  either  an  ID, or  the  creature
;;   object.")) (:documentation "A  base class used for  any entity (eg,
;;   player) who can control a character in the game in some way."))

(datafly:defmodel player
  "A  structure representing  a  player  who is  a  human  being in  the
 real world.

Structure slots:

@itemize  @item ID

A 128-bit  unique identifier  for this player,  used internally.  We are
generating these using v4-UUID's, at  least for now, although other than
being a positive  integer, I would not recommend  making any assumptions
about their internal format.

@item EMAIL

The primary  e-mail address  associated with  this player.  The player's
contact  methods list  may have  alternate  addresses, and  there is  no
guarantee that  this “primary” address is  as good as, much  less better
than, any of them.  This value may be used for  display only, but before
actually  sending mail,  visit the  contact  methods list  and find  the
actual best address.

This may also be NIL.

@item GIVEN-NAME

The player's given name (usually, first  name) if one is known. This may
be NIL.

@item SURNAME

The  player's surname  (family  name, or  last name)  if  one is  known.
This may be NIL.

@item FULL-NAME

The player's  full name, formatted as  they like it. This  might include
honorifics or titles;  there is no safe way to  convery between this and
the given name and surname.

@item DATE-OF-BIRTH

The player's date of birth, if it is known.

@item AGE

The  player's age,  if it  is known.  Note that  the reader  method will
compute the age from the date-of-birth when one is present.

For sanity-checking purposes only, the age  is clamped to 0…130. This is
mostly to detect the not-uncommon  data-entry error of someone supplying
a  two-digit year-of-birth  that  is later  interpreted  as their  being
1,900+ years old.

@end itemize"
  (id (binary<-uuid (uuid:make-v4-uuid))
      :type (unsigned-byte 128))
  (email nil :type (or null string))
  (given-name nil :type (or null string))
  (surname nil :type (or null string))
  (full-name nil :type (or null string))
  (date-of-birth nil :type (or null local-time:date))
  (age nil :type (or null (real 0 (130)))))

(defun legal-age (date-of-birth &optional (reference-date (local-time:now)))
  "The age of  a person born on DATE-OF-BIRTH, as  of REFERENCE-DATE (or
right  now).  This uses  the  legal  definition  that the  person's  age
increments at  the midnight of their  date of birth each  year, with the
date 29 February treated as 1 March on non-leap-years.

The time  zone used for  this computation  is the not  defined, however,
yielding  rather  irregular  behaviour   depending  on  time  zones  and
the like.

TODO: Determine  in what  time zone  we should  computer this  for legal
reasons, eg, COPPA."
  (check-type date-of-birth local-time:timestamp)
  (check-type reference-date local-time:timestamp)
  (unless (local-time:timestamp< date-of-birth reference-date)
    (return-from legal-age 0))
  (multiple-value-bind (msec sec min hour day month year)
      (local-time:decode-timestamp reference-date)
    (declare (ignore msec sec min hour))
    (multiple-value-bind (msec sec min hour
                               day-of-birth month-of-birth year-of-birth)
        (local-time:decode-timestamp date-of-birth)
      (declare (ignore msec sec min hour))
      (let ((had-birthday-p (or (< month-of-birth month)
                                (and (= month-of-birth month)
                                     (<= day-of-birth day)))))
        (+ (- year
              year-of-birth
              1)
           (if had-birthday-p 1 0))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; Promote the  DEFSTRUCT function to  a generic function that  we can
  ;; define a method AROUND.
  (warn "Redefining PLAYER-AGE around ~a"
        (symbol-function 'player-age))
  (let ((player-age-fn (symbol-function 'player-age)))
    (unless (typep player-age-fn 'generic-function)
      (fmakunbound 'player-age))
    (defgeneric player-age (player)
      (:documentation "Get the AGE of the PLAYER"))
    (when player-age-fn
      (defmethod player-age ((player player))
        (funcall player-age-fn player)))))

(defmethod player-age :around (player)
  "This  AROUND method  will define  the PLAYER's  age based  upon their
date-of-birth   (`PLAYER-DATE-OF-BIRTH')   rather    than   the   stored
`PLAYER-AGE' slot-value, when the date of birth is known."
  (let ((dob (player-date-of-birth player)))
    (if dob
        (let ((legal-age (legal-age dob))
              (recorded-age (call-next-method)))
          (unless (= legal-age recorded-age)
            (setf (player-age player) legal-age))
          legal-age)
        (call-next-method))))

(defun player-adult-p (player)
  "Reurns a generalized true value if PLAYER is 18 or older.

Should return true beginning on the day of their 18th birthday."
  (>= (player-age player) 18))

(defun player-under-13-p (player)
  "Returns a generalized true value if PLAYER is under 13.

Should return NIL beginning on the day of their 13th birthday."
  (< (player-age player) 13))

(defun player-over-13-p (player)
  "Returns a generalized true value if PLAYER is over 13.

Should return true beginning on the day of their 13th birthday."
  (>= (player-age player) 13))

;; (defun  class-slot-writers (class)  "Enumerate the  first writer  (if
;;   any) for  any slot on CLASS  that has one." (loop  with not-found =
;;   'not-found for slot  in (closer-mop:class-slots (find-class class))
;;   for  writers   =  (closer-mop:slot-definition-writers   slot)  when
;;   writers collect (cons slot (first writers))))

;; (eval-when  (:compile-toplevel  :execute) (dolist  (class  '(player))
;;   (dolist    (writer    (class-slot-writers    class))    (add-method
;;   (closer-mop:method-generic-function writer) (make-method )))))


(defun update-player-info (player-id info)
  "GIven  property  list  INFO,  find  the record  for  player  with  ID
PLAYER-ID and update  each slot-value whose :initarg values  are keys in
INFO, with the value following it in the list.

In other  words, update fields in  the player record by  overwriting the
fields mentioned in INFO in much hte same way as they woudl be passed to
MAKE-INSTANCE 'PLAYER."
  (let ((Δ (plist-alist info)))
    (when Δ
      (let ((player (find-player-by-id player-id)))
        (loop for (key . new-value) in Δ
           do (setf (slot-value (intern (symbol-name key)) player) new-value))))))

(defun link-player-to-registration (player registrar id-string)
  "Link  a  PLAYER object  to  a  login  REGISTRAR and  their  ID-STRING
representing that player."
  (check-type player player)
  (check-type registrar (or string symbol) "string-designator for a registrar")
  (check-type id-string string
              "string that uniquely identifies a user across time, distinctive to REGISTRAR")
  (with-connection :members
    (datafly:execute
     (sxql:insert-into :player-registrations
       (sxql:set= :player-id (player-id player)
                  :registrar (string registrar)
                  :id-string id-string)))))

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
MAKE-USER-REGISTRATION  to  instantiate  a  record.  Otherwise,  methods
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
  (with-connection :members
    (datafly:retrieve-one-value
     (sxql:select (:id)
       (sxql:from :player-registrations)
       (sxql:where (:and (:equal :registrar (string registrar))
                         (:equal :id-string id-string)))))))

(defmethod user-id<-registrar-id ((registrar symbol) (id string) &optional info)
  "The default handler  accepts any persistently unique  ID string and
 returns a user object, if one exists."
  (declare (ignore info))
  (user-id<-registration registrar id))

(defun find-player-by-id (id)
  "Retrieve the player object represented by ID."
  (check-type id (unsigned-byte 128))
  (with-connection :members
    (datafly:retrieve-one
     (sxql:select (:*)
       (sxql:from :players)
       (sxql:where (:= :id id)))
     :as :plist)))

(defun ensure-player (player)
  "Ensure  that  PLAYER   is  a  PLAYER  object.   Coërces  integers  or
base-36-coded integer strings."
  (etypecase player
    (player player)
    ((integer 1 *) (find-player-by-id player))
    (string (find-player-by-id (parse-integer player :radix 36)))))
