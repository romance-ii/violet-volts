(defpackage parenthetical
  (:use :cl :hunchentoot :cl-who :smackjack :oliphaunt)
  (:export :server)
  (:shadowing-import-from :ps #:@))

(defpackage blitzen (:use :parenscript :smackjack))
(in-package :parenthetical)

(setf ps:*js-string-delimiter* #\")	; Allow cl-who and parenscript to work together

(defparameter *public-root* #p "/home/brpocock/Projects/violet-volts/Parenthetical/src/static/")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/action"))

(defun-ajax evaluate (data) (*ajax-processor* :callback-data :response-text)
  (format nil "~s" (eval (read-from-string data))))

(defun-ajax log-in (e-mail nickname password)
    (*ajax-processor* :callback-data :response-text)
  (when (and (or (not (emptyp e-mail))
                 (not (emptyp nickname)))
             (not (emptyp password)))
    (map 'string #'code-char (loop repeat 32 collect (+ 32 (random 95))))))

(define-easy-handler (dir :uri "/") ()
  (with-html-output-to-string (s
                               ;; :prologue "<!DOCTYPE html>" :indent 2
                               )
    (:html
        (:head (:title "Comet Test Spike")
               (:link :rel "/static/donder.css" :type "text/css"))
      (:body (:h1 "Comet Test")
             (:ul
              (:li (:a :href "/repl" :title "Read-Eval-Print-Loop"
                       (str "REPL")))
              (:li (:a :href "/cupid.html" :title "Game Client Test"
                       (str "Game Client Test"))))))))

(define-easy-handler (client.js :uri "/client/parenthetical.js") ()
  (setf (header-out :content-type) "text/javascript")
  (with-output-to-string (ps:*parenscript-stream*)
    (princ "/* -*- js -*- */ \"use strict\";
/*
 * Parenthetical — Generated Javascript Interface
 *
 * The contents of this file are the compiled form of the Parenthetical
 * client libraries. This program is free software: you can use and/or
 * modify it in it accordance with the terms of the GNU Affero General
 * Public License (AGPL).  The source code of this program consists 
 * of the Parenscript code which is compiled to produce this file.
 * 
 * See http://star-hope.org/software/Parenthetical/ for details.
 *
 * Copyright © 2016, Bruce-Robert Fenn Pocock.
 */"
           ps:*parenscript-stream*)
    (princ (generate-prologue *ajax-processor* :wrapper nil) ps:*parenscript-stream*)
    (dolist (file (directory (merge-pathnames #p "../ps/*.paren" *public-root*)))
      (with-input-from-file (*standard-input* file)
        (format ps:*parenscript-stream* "~&/* Begin: ~s */~%" (pathname-name file))
        (let ((*package* (find-package :blitzen)))
          (ps:ps-compile-stream *standard-input*))
        (format ps:*parenscript-stream* "~&/* End: ~s */~%" (pathname-name file))))
    (princ "/* End of parenthetical.js */" ps:*parenscript-stream*)))

(define-easy-handler (repl :uri "/repl") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Comet REPL")
      (str (generate-prologue *ajax-processor*))
      (:script :type "text/javascript"
               (str
                (ps:ps
                  (defun callback (response)
                    (setf (@ ((@ document get-element-by-id) "replies") value)
                          (+ (@ ((@ document get-element-by-id) "replies") value)
                             #\newline
                             " ⇒ "
                             response)))
                  (defun error-callback (response)
                    (alert response))
                  (defun on-click ()
                    ((@ smackjack evaluate) (@ ((@ document get-element-by-id) "data") value)
                     callback error-callback)
                    (setf (@ ((@ document get-element-by-id) "data") value) "")
                    (@ ((@ document get-element-by-id) "data") set-focus))))))
     (:body :style "margin: 0; padding: 0; font-family: Cantarell, serif"
            (:div
             (:textarea :id "replies" :style "height: calc(100% - 2em); width: 100%; font-family: Cantarell, serif;"
                        (str "This is incredibly stupid, by the way. Feel free to destroy the remote server.")))
            (:div 
             (:input :id "data" :type "text" :style "width: calc(100% - 5em); background-color: navy; color: yellow;"
                     :value "(format nil \"~& You have remote access to Common Lisp (~a) running on a ~a machine named ~:(~a~).\" (lisp-implementation-type) (machine-type) (machine-instance))"
                     :onblur (ps:ps-inline (on-click)))
             (:button :type "button" :style "padding: 0; text-align: center; width: 4em" ;
                      :onclick (ps:ps-inline (on-click))
                      "eval"))))))

(defparameter *server-lock* (make-lock "Server process access lock"))
(defparameter *server* nil)

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-ajax-dispatcher *ajax-processor*)))

(defun stop-server ()
  (with-lock-held (*server-lock*)
    (when *server*
      (ignore-errors 
        (hunchentoot:stop *server* :soft t))
      (setf *server* nil))))

(defun start-server ()
  (with-lock-held (*server-lock*)
    (setf *server* (make-instance 'easy-acceptor
                                  :name "Olive"
                                  :address "localhost" 
                                  :port 2770
                                  :document-root *public-root*)) 
    (let ((hunchentoot:*catch-errors-p* nil)
          (hunchentoot:*show-lisp-errors-p* t))
      (start *server*))))

(defun server ()
  (stop-server)
  (start-server))
