;;;; version.lisp — version info
(in-package :tootstest.web)

(defun romance-ii-program-version ()
  "This program's version. Taken from ASDF."
  (asdf:component-version (asdf:find-system :tootstest)))

(defun romance-ii-program-name ()
  "This program's name. Taken from ASDF."
  (asdf:component-name (asdf:find-system :tootstest)))

(defun romance-ii-program-name/version ()
  "This program's name and version number, in name/version form, as used
in HTTP headers and such."
  (concatenate 'string
               (romance-ii-program-name)
               "/"
               (romance-ii-program-version)))

(defroute route-/version "/tootstest/version" ()
          "Render the version and license information nicely for the player."
          (let ((version-info-list
                 (list :product (romance-ii-program-name)
                       :version (romance-ii-program-version)
                       :machine (list :version (machine-version)
                                      :type (machine-type)
                                      :instance (string-capitalize (machine-instance)))
                       :lisp (list :type (lisp-implementation-type)
                                   :version (lisp-implementation-version))
                       :software (list :type (software-type)
                                       :version (software-version))
                       :acceptor (list :name (request-server-name *request*)
                                       :port (request-server-port *request*)
                                       :protocol (request-server-protocol *request*))
                       :copyright-latest #.(local-time:format-timestring
                                            nil (local-time:now)
                                            :format '(:year))
                       :build-date #.(local-time:format-timestring
                                      nil (local-time:now)
                                      :format '(:year #\- :month #\- :day)))))
            (cond
              ((wants-json-p) (render-json (plist-alist version-info-list)))
              (t (render #p"version.html" version-info-list)))))
