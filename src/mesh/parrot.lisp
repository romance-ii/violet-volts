(in-package :parrot)

(defun stringy (thing)
  (cond ((stringp thing) thing)
        ((consp thing) (reduce #'string& thing))
        (t (princ-to-string thing))))

(defun string& (s₀ s₁)
  (concatenate 'string (stringy s₀) (stringy s₁)))

(defparameter *squawk* "System Message:")
(defparameter *wiki-prefix* "https://wikiwiki.tootsville.adventuring.click/")

(defun make-squawk-message (title message wiki-page)
  (reduce #'string&
          (list *squawk* #(#\Newline #\Newline)
                (if message
                    (list title #(#\: #\Newline) message)
                    title)
                (when wiki-page
                  (list #(\Newline #\Newline)
                        "Need more info? There's helpful document titled “"
                        (substitute #\Space #\_ wiki-page)
                        "”")))))

(defun squawk (title &optional message wiki-page)
  (let ((message (make-squawk-message title message wiki-page)))
    (#j:console:log message)
    (if wiki-page
        (when (#j:confirm message)
          (setf #j:window:location
                (concatenate 'string *wiki-prefix* wiki-page)))
        (#j:alert message))))

(defun make-peep-message (message hookp)
  (reduce #'string& (list message
                          (when hookp
                            (list )))))

(defun peep (message &optional hook)
  (let ((message (make-peep-message message)))
    (if hook
        (when (#j:confirm message)
          (funcall hook))
        (#j:alert message))))
