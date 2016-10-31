;;;;; zomg.lisp —- Client exceptions reporter
(in-package :tootstest.web)

(defroute route-/zomg "/tootstest/zomg" ()
  "Zombies! Oh, my God! — The  client-side can report its crashes to the
host-side, here. The crash report should arrive in JSON format."
  (assert (string-equal +application/json+ (request-content-type *request*)
                        :end2 #.(length +application/json+))
          () "Zombies should always use JSON, not ~a" (request-content-type *request*))
  (let ((report (let ((yason:*parse-object-as* :plist)
                      (yason:*PARSE-json-arrays-as-vectors* t))
                  (yason:parse (request-raw-body *request*)))))
    (format *error-output* "ZOMG! error report from client:~%~s" report)))
