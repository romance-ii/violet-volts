(in-package :cl-user)
(defpackage tootstest-asd
  (:use :cl :asdf))
(in-package :tootstest-asd)

(defsystem tootstest
  :version "0.1"
  :author "Bruce-Robert Fenn Pocock <BRFennPocock@star-hope.org>"
  :license "Private test"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                        :components
                        ((:file "main" :depends-on ("config" "view" "db"))
                         (:file "web" :depends-on ("view"))
                         (:file "gossip" :depends-on ("web"))
                         (:file "meta-game" :depends-on ("web"))
                         (:file "characters" :depends-on ("web"))
                         (:file "view" :depends-on ("config"))
                         (:file "db" :depends-on ("config"))
                         (:file "config"))))
  :description "tootstest"
  :in-order-to ((test-op (load-op tootstest-test))))
