(in-package :cl-user)

(asdf:defsystem :tootstest-test
  :author "Bruce-Robert Fenn Pocock <BRFennPocock@star-hope.org>"
  :license "AGPLv3"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:tootstest
               :selenium
               :prove)
  :components ((:module "t"
                        :components
                        ((:test-file "tootstest")
                         (:test-file "selenium-test")))) 
  :perform (asdf:test-op :after (op c)
                         (funcall (intern #.(string :run) :prove) :tootstest-test))
  :perform (asdf:load-op :after (op c) (asdf:clear-system c)))
