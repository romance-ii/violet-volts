(in-package :cl-user)
(defpackage tootstest.config
  (:use :cl)
  (:import-from :envy
   :config-env-var
                :defconfig)
  (:export :config
   :*application-root*
           :*static-directory*
   :*template-directory*
           :appenv
   :developmentp
           :productionp))
(in-package :tootstest.config)

(setf (config-env-var) "VIOLETVOLTS")

(defparameter *application-root*
  (asdf:system-source-directory :tootstest)
  "The directory in which this application is installed.")
(defparameter *static-directory* 
  (asdf:system-relative-pathname :tootstest #P"static/")
  "The directory in which static assets are kept")
(defparameter *template-directory*
  (asdf:system-relative-pathname :tootstest #P"templates/")
  "The directory to search for HTML templates")

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
      :on-error-mail (:from-name "Tootsville Support"
                      :from-address "support@tootsville.adventuring.click"
                      :to-address "support@tootsville.adventuring.click"
                      :smtp-server "localhost"
                      :subject-prefix "Error")))

(defconfig |development|
    '())

(defconfig |production|
    '(:error))

(defconfig |test|
    '())

(defun config (&optional key)
  "Obtain KEY from the active configuration scheme"
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  "Obtain the  environment name  under which  the application  should be
running   from    the   Unix   environment   variable    determined   by
`ENVY:CONFIG-ENV-VAR', which is set to ROMANCE2ENV for this package."
  (uiop:getenv (config-env-var #.(package-name *package*))))

(setf (config-env-var #.(package-name *package*)) "ROMANCE2ENV")

(defun developmentp ()
  "True if the `APPENV' is development."
  (string= (appenv) "development"))

(defun productionp ()
  "True if the `APPENV' is production."
  (string= (appenv) "production"))
