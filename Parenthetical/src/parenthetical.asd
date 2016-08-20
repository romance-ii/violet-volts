(cl:in-package :cl-user)
(require 'asdf)
(asdf:defsystem :parethetical
  :description "Parenthitical: Game client for browsers"
  :author "Bruce-Robert Fenn Pocock"
  :version "0.1"
  :depends-on (:oliphaunt 
               
               :cl-who
               :hunchentoot
               :parenscript
               :smackjack
               
               )
  :encoding :utf-8
  
  :components 
  ((:module "parenthetical"
            :components ((:file "comet")))))
