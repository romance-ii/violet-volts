;;; load jscl.lisp first and be in the JSCL CWD
(in-package :jscl)

(defun bootstrap-mesh ()
  (bootstrap)
  (let ((*features* (list* :jscl :jscl-xc *features*))
        (*package* (find-package "JSCL"))
        (*default-pathname-defaults* *base-directory*)) 
    (compile-application
     (directory (source-pathname "*" :directory '(:relative :up :up :up "src" "mesh")
                                 :type "lisp")))))

