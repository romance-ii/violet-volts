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

(defun unembarassing (string)
  (loop for ((from to)) on '(("\\(R\\)" "®") ("\\(TM\\)" "™"))
     do (setf string
              (cl-ppcre:regex-replace-all from string to)))
  string)

(defun version-info-list ()
  (let ((basics
         (list :product (romance-ii-program-name)
               :version (romance-ii-program-version)
               :machine (list :version (unembarassing (machine-version))
                              :type (machine-type)
                              :instance (string-capitalize (machine-instance)))
               :lisp (list :type (lisp-implementation-type)
                           :version (lisp-implementation-version))
               :software (list :type (software-type)
                               :version (software-version))
               :copyright-latest #.(local-time:format-timestring
                                    nil (local-time:now)
                                    :format '(:year))
               :build-date #.(local-time:format-timestring
                              nil (local-time:now)
                              :format '(:year #\- :month #\- :day))
               :compiled tootstest::*compiled*)))
    (if *request*
        (list* :acceptor (list :name (request-server-name *request*)
                               :port (request-server-port *request*)
                               :protocol (request-server-protocol *request*))
               basics)
        basics)))

(defroute route-/version ("/version" :accept '("text/html" "*/*")) ()
          "Render the version and license information nicely for the player."
          (render #p"version.html" (version-info-list)))

(defroute route-/version/json ("/version" :accept '("application/json")) ()
          (render-json (plist-alist (version-info-list))))

(defroute route-/version.json "/version.json" ()
          (setf (reply-external-format*) "utf-8")
          (render-json (plist-alist (version-info-list))))

(defroute route-/version.txt "/version.txt" ()
          (format nil "~{~:(~24a~): ~a~%~}" (version-info-list)))

(defun extract-plist-path (path plist &optional prefix)
  (labels ((prefixed (key)
             (if prefix
                 (concatenate 'string prefix "/"
                              (string key))
                 (string key))))
    (etypecase path
      (null (if (consp plist)
                (loop for (key . value) on plist by #'cddr
                   append (extract-plist-path nil (car value)
                                              (prefixed key)))
                (list prefix plist)))
      (cons (extract-plist-path (rest path) (getf plist (first path))
                                (prefixed (first path))))
      (symbol (let ((value (getf plist path)))
                (if (consp value)
                    (extract-plist-path nil value (prefixed path))
                    (list (prefixed path) value)))))))

(assert (equalp (extract-plist-path nil '(:a (:b 42 :c 99) :x 0))
                '("A/B" 42 "A/C" 99 "X" 0)))
(assert (equalp (extract-plist-path '(:a :b) '(:a (:b 42)))
                '("A/B" 42)))
(assert (equalp (extract-plist-path :a '(:a (:b 42 :c 99) :x 0))
                '("A/B" 42 "A/C" 99 )))
(assert (equalp (extract-plist-path '(:a) '(:a (:b 42 :c 99) :x 0))
                '("A/B" 42 "A/C" 99 )))

(defun version-info-for (args)
  (let ((keys (mapcar (lambda (name)
                        (intern (string-upcase name) :keyword))
                      (or args '(:*))))
        (info (version-info-list)))
    (loop for key in keys
       appending
         (cond
           ((find #\/ (string key))
            (extract-plist-path (mapcar #'make-keyword
                                        (split-sequence #\/ (string key)))
                                info))
           ((equal :* key)
            (extract-plist-path nil info))
           (t (list key (getf info key)))))))

(defun tootstest::version-info-report (args)
  (let ((info (version-info-for args)))
    (cond ((and (= 2 (length info))
                (= 1 (length args))
                (string-equal (first args) (first info)))
           (princ (second info))
           (terpri))
          (t (format t "~{~:(~a~):	~a~%~}" info)))
    (finish-output)))
