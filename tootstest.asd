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
               :uuid
               :yason
               )
  :components
  ((:module "src"
            :components
            ((:file "main" :depends-on ("config" "view" "db"))
             (:file "db-player" :depends-on ("db"))
             (:file "web" :depends-on ("view" "db-player"))

             (:file "login" :depends-on ("web"))
             (:file "static" :depends-on ("web"))
             (:file "register" :depends-on ("web"))
             (:file "version" :depends-on ("web"))
             (:file "zomg" :depends-on ("web"))
             (:file "gossip" :depends-on ("web"))
             (:file "script" :depends-on ("web"))
             (:file "news" :depends-on ("web"))
             (:file "meta-game" :depends-on ("web"))
             (:file "characters" :depends-on ("web"))

             (:file "view" :depends-on ("config"))
             (:file "db" :depends-on ("config"))
             (:file "config")

             (:module "mesh"
                      :components
                      ((:file "assets/archives" :depends-on ("package"))
                       (:file "assets" :depends-on ("package"))
                       (:file "assets/service" :depends-on ("package"))
                       (:file "assets/torrent" :depends-on ("package"))
                       (:file "babylon" :depends-on ("package"))
                       (:file "chatter" :depends-on ("package"))
                       (:file "device/ambient-light" :depends-on ("package"))
                       (:file "device/network" :depends-on ("package"))
                       (:file "device/orientation" :depends-on ("package"))
                       (:file "device/vibration" :depends-on ("package"))
                       (:file "gossipnet/events" :depends-on ("package"))
                       (:file "gossipnet/genesis" :depends-on ("package"))
                       (:file "gossipnet/integrity" :depends-on ("package"))
                       (:file "gossipnet/net" :depends-on ("package"))
                       (:file "login" :depends-on ("package"))
                       (:file "package")
                       (:file "parrot/buddy-list" :depends-on ("package"))
                       (:file "parrot/child-account" :depends-on ("package"))
                       (:file "parrot/child-login" :depends-on ("package"))
                       (:file "parrot" :depends-on ("package"))
                       (:file "parrot/new-player" :depends-on ("package"))
                       (:file "player-input/bluetooth-gamepad" :depends-on ("package"))
                       (:file "player-input/bluetooth-wiimote" :depends-on ("package"))
                       (:file "player-input/events" :depends-on ("package"))
                       (:file "player-input/gamepad" :depends-on ("package"))
                       (:file "player-input/keyboard" :depends-on ("package"))
                       (:file "player-input/lipread" :depends-on ("package"))
                       (:file "player-input/listen" :depends-on ("package"))
                       (:file "player-input/tabletpad" :depends-on ("package"))
                       (:file "player-input/touch" :depends-on ("package"))
                       (:file "romans/Aelius-Galenus/galen" :depends-on ("package"))
                       (:file "romans/Appius-Claudius-Caecus/appius" :depends-on ("package"))
                       (:file "romans/Clodia-Metelli-Pulcher/clodia" :depends-on ("package"))
                       (:file "romans/Gaius-Asinius-Pollio/asinius" :depends-on ("package"))
                       (:file "romans/Gaius-Julius-Caesar/caesar" :depends-on ("package"))
                       (:file "romans/Gaius-Lutatius-Catulus/lutatius" :depends-on ("package"))
                       (:file "romans/Lucius-Aemilius-Regillus/regillus" :depends-on ("package"))
                       (:file "romans/Marcus-Vitruvius-Pollio/vitruvius" :depends-on ("package"))
                       (:file "romans/Narcissus/narcissus" :depends-on ("package"))
                       (:file "romans/Publius-Cornelius-Tacitus/tacitus" :depends-on ("package"))
                       (:file "romans/Rabirius/rabirius" :depends-on ("package"))
                       (:file "romans/Rahab/rahab" :depends-on ("package"))
                       (:file "romans/Sextus-Julius-Frontinus/frontinus" :depends-on ("package"))
                       (:file "ux/events" :depends-on ("package"))
                       (:file "ux/gossip-mouse" :depends-on ("package"))
                       (:file "ux/make-noise" :depends-on ("package"))
                       (:file "ux/overlay" :depends-on ("package"))
                       (:file "ux/parrot" :depends-on ("package"))
                       (:file "ux/speak" :depends-on ("package"))
                       (:file "violet-volts" :depends-on ("package"))
                       (:file "webdebug" :depends-on ("package"))
                       (:file "webinspect" :depends-on ("package"))
                       (:file "webrepl" :depends-on ("package"))
                       (:file "world/events" :depends-on ("package"))
                       (:file "xhr" :depends-on ("package")))))))
  :description "tootstest"
  :in-order-to ((test-op (load-op tootstest-test))))
