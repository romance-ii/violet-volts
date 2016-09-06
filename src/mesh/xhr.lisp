(in-package :xhr)

(defun get (uri callback &optional headers)
  (error "No get"))

(defmacro with-get ((uri response &optional headers) 
                    &body body)
  `(get ,uri (lambda (,response) ,@body nil) ,headers))

(defun post (uri callback &optional fields headers)
  (error "No post"))

(defmacro with-post ((uri response &optional fields headers)
                     &body body)
  `(post ,uri (lambda (,response) ,@body nil) ,fields ,headers))
