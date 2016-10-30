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

(defclass creature-controller ()
  ((creature :type (or creature (unsigned-byte 128) null)
             :accessor creature-controller-creature 
             :initarg :creature
             :documentation "If this controller is currently controlling
             a creature, this is a reference to that creature; either an
             ID, or the creature object."))
  (:documentation "A base class used for any entity (eg, player) who can
 control a character in the game in some way."))

(defclass player (creature-controller)
  ((player-id :type (unsigned-byte 128) :accessor player-id
              :initform (binary<-uuid (uuid:make-v4-uuid))
              :documentation  "A  128-bit  unique  identifier  for  this
player, used  internally. We  are generating  these using  v4-UUID's, at
least for now, although other than being a positive integer, I would not
recommend making any assumptions about their internal format.")
   (player-email :type (or string null) :accessor player-email
                 :initform :email
                 :documentation "The  primary e-mail  address associated
with this player.  The player's contact methods list  may have alternate
addresses, and there  is no guarantee that this “primary”  address is as
good as, much less better than, any  of them. This value may be used for
display  only,  but before  actually  sending  mail, visit  the  contact
methods list and find the actual best address.

This may also be NIL.")
   (player-given-name :type (or string null) :accessor player-given-name
                      :initform :given-name
                      :documentation "The player's  given name (usually,
first name) if one is known. This may be NIL.")
   (player-surname :type (or string null) :accessor player-surname
                   :initform :surname
                   :documentation "The player's surname (family name, or
last name) if one is known. This may be NIL.")
   (player-full-name :type (or string null) :accessor player-full-name
                     :initform :full-name
                     :documentation "The  player's full  name, formatted
as they  like it. This might  include honorifics or titles;  there is no
safe way to convery between this and the given name and surname.")
   (player-date-of-birth :type (or local-time:date null) :accessor player-date-of-birth
                         :initform :date-of-birth
                         :documentation "The player's date of birth, if it is known.")
   (player-age :type (or (real 0 *) null) :accessor player-age
               :initform :age
               :documentation  "The  player's  age,   if  it  is  known.
Note that the reader method will  compute the age from the date-of-birth
when one is present."))
  (:documentation "A class representing a player who is a human being in
 the real world."))



(defun legal-age (date-of-birth)
  "The age of  a person born on DATE-OF-BIRTH, right  now. This uses the
legal definition  that the  person's age increments  at the  midnight of
their date  of birth  each year,  with the date  29 February  treated as
1 March on non-leap-years.

The time  zone used for  this computation  is the not  defined, however,
yielding  rather  irregular  behaviour   depending  on  time  zones  and
the like.

TODO: Determine  in what  time zone  we should  computer this  for legal
reasons, eg, COPPA."
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
  "GIven  property  list  INFO,  find  the record  for  player  with  ID
PLAYER-ID and update  each slot-value whose :initarg values  are keys in
INFO, with the value following it in the list.

In other  words, update fields in  the player record by  overwriting the
fields mentioned in INFO in much hte same way as they woudl be passed to
MAKE-INSTANCE 'PLAYER."
  (let ((Δ (collect-setters-for-class 'player info)))
    (when Δ
      (let ((player (find-player-by-id player-id)))
        (loop for (writer . new-value) in Δ
           do (funcall writer player new-value))))))



(defun link-player-to-registration (player registrar id-string)
  "Link  a  PLAYER object  to  a  login  REGISTRAR and  their  ID-STRING
representng that player."
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
