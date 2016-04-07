(defpackage compile-blitzen
  (:use :cl :alexandria))
(in-package :compile-blitzen)
(require 'parenscript)
(let ((js (merge-pathnames #p"blitzen.js"
                           (or *load-pathname*
                               *compile-file-pathname*)))
      (ps (merge-pathnames #p"blitzen.ps"
                           (or *load-pathname*
                               *compile-file-pathname*))))
  (with-output-to-file (*standard-output* js :if-exists :supersede)
    (princ (ps:ps-compile-file ps))))

