(in-package :tootstest.web)
(syntax:use-syntax :annot)

(defroute "/characters/" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  '(:error))
