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

(defparameter *application-root* (asdf:system-source-directory :tootstest))
(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

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
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
