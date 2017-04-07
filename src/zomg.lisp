;;;;; zomg.lisp —- Client exceptions reporter
(in-package :tootstest.web)

(defroute route-/zomg ("zomg" :method :post :content-type '("application/json"))
  ()
  "Zombies! Oh, my God! — The client-side can report its crashes
to the host-side, here. The crash report should arrive in JSON format.
\(For now, we're just “cheating” using Rollbar to do this, for us.)"
  (let ((report (let ((yason:*parse-object-as* :plist)
                      (yason:*PARSE-json-arrays-as-vectors* t))
                  (yason:parse (request-raw-body *request*)))))
    (format *error-output* "ZOMG! error report from client:~%~s" report)))

(defroute route-/zomg ("zomg" :method :post)
  ()
  (format nil "Zombies should always use JSON, not ~a"
          (request-content-type *request*)))
