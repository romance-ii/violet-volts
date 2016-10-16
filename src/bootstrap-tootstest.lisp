;;; load jscl.lisp first and be in the JSCL CWD
(in-package :jscl)

(defun bootstrap-mesh ()
  (bootstrap-core)
  (let ((*features* (list* :jscl :jscl-xc *features*))
        (*package* (find-package "JSCL"))
        (*default-pathname-defaults* *base-directory*)) 
    (compile-application
     (cons
      (source-pathname "package" :directory '(:relative :up :up :up "src" "mesh")
                       :type "lisp")
      (remove-if
       (lambda (path)
         (equal (pathname-name path) "package"))
       (directory (source-pathname "*" :directory '(:relative :up :up :up "src" "mesh")
                                   :type "lisp"))))
     (source-pathname "mesh" :directory '(:relative :up :up :up "js")
                      :type "js"))))

