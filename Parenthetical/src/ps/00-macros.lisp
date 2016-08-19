;; -*- lisp -*-

;;(defpackage "VIOLET-VOLTS"
;;(:use :parenscript :common-lisp)
;;)


;;(in-package "VIOLET-VOLTS")

;; (handler-bind
;;     ((name-conflict
;;       (lambda (c)
;;         (declare (ignorable c))
;;         (when (find-restart 'sb-impl::take-new)
;;           (invoke-restart 'sb-impl::take-new))
;;         (format *error-output*
;;                 "~& No TAKE-NEW restart on NAME-CONFLICT; ~&~S 
;;  Found: ~{~% • ~S ~}~%" c (compute-restarts)))))
;;   (use-package :ps))

(setf parenscript::*js-string-delimiter* #\'
      parenscript::*js-target-version*   1.6)

(ps:ps (defmacro by-id (id)
         `((@ document get-element-by-id) ,id)))

(ps:ps (defmacro note (&rest params)
         `((@ console log) ,@params)))

(ps:ps (defmacro concat (&rest stuff)
         (append (list 'concatenate ''string) stuff)))

(ps:ps (defmacro fapply (function &rest stuff)
         `(,function ,@stuff)))

(ps:ps (defmacro 0+ (thing) `(+ 0 ,thing)))

(ps:ps (defmacro get-unix-time () '(0+ (new *date))))

(ps:ps (defmacro console-log (&rest stuff) `((@ console log) ,@stuff)))

(ps:ps (defmacro mod (number base) `(% ,number ,base)))

(ps:ps (defmacro zerop (number) `(= 0 ,number)))

(ps:ps (defmacro if-let (((var init-form)) &body body) `(let ((,var ,init-form)) (if ,var ,@body))))

