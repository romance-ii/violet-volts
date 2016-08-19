(in-package :cl-user)
(defpackage tootstest-test-asd
  (:use :cl :asdf))
(in-package :tootstest-test-asd)

(defsystem tootstest-test
  :author "Bruce-Robert Fenn Pocock <BRFennPocock@star-hope.org>"
  :license ""
  :depends-on (:tootstest
               :prove)
  :components ((:module "t"
                :components
                ((:file "tootstest"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
