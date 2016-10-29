(in-package :cl-user)
(defpackage tootstest-asd
  (:use :cl :asdf))
(in-package :tootstest-asd)

(defsystem tootstest
  :version "0.2"
  :author "Bruce-Robert Fenn Pocock <BRFennPocock@star-hope.org>"
  :license "AGPL v3+"
  :bug-tracker "https://github.com/romance-ii/violet-volts/issues"
  :description
  "A test server to initiate the Tootsville mesh networking. Part of the Romance Ⅱ project."
  :long-description
  " The  tootstest server is  integrating the mesh networking  code with
the components from  the Romance Ⅱ development, using  Tootsville as the
testbed case for the development."
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
               :sxql

               :bordeaux-threads
               :cl-uglify-js
               :cl-rdfxml
               :drakma
               :yason
               
               :uuid
               )
  :components ((:module "src"
                        :components
                        ((:file "main" :depends-on ("config" "view" "db"))
                         (:file "web" :depends-on ("view"))
                         (:file "gossip" :depends-on ("web"))
                         (:file "script" :depends-on ("web"))
                         (:file "meta-game" :depends-on ("web"))
                         (:file "characters" :depends-on ("web"))
                         (:file "view" :depends-on ("config"))
                         (:file "db" :depends-on ("config"))
                         (:file "config"))))
  :description "tootstest"
  :in-order-to ((test-op (load-op tootstest-test))))
