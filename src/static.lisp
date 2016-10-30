(in-package :tootstest.web)

;;; Static assets

(defun send-static (content-type pathname)
  "Send a  static file  as a  response to  a query.  This implementation
buffers the  entire file  into RAM, so  it should not  be used  for very
large files. There is currently  no caching whatsoever, either, although
the OS may do some disc caching."
  (check-type content-type string "A MIME content-type, eg, text/html;charset=utf-8")
  (check-type pathname (or string pathname)
              "The pathname ofa local file to be sent.")
  (appendf (response-headers *response*)
           (list "Content-Type" content-type))
  (read-file-into-string pathname))

(defroute "/tootstest/css/:name.css" (&key name)
  "CSS files are served statically."
  (send-static "text/css;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "css")
                                 :name name :type "css")
                                *static-directory*)))

(defroute "/tootstest/js/:name.js" (&key name)
  "JavaScript files are served statically."
  (send-static "application/javascript;charset=utf-8"
               (merge-pathnames (make-pathname
                                 :directory '(:relative "js")
                                 :name name :type "js")
                                *static-directory*)))
