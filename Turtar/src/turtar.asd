;;; -*- lisp -*-

(require 'asdf)
(in-package :cl-user)
(asdf:defsystem :turtar
  :description "Turtar"
  :author "Bruce-Robert Fenn Pocock"
  :version "0.1"
  :maintainer "Bruce-Robert Fenn Pocock"
  
  :depends-on (:alexandria
               :closer-mop
               :uuid
               
               :oliphaunt)
  
  :encoding :utf-8
  
  :components
  ((:file "turtar")
   (:module "entities"
            :depends-on ("turtar")
            :components ((:file "core-entity")
                         (:file "creature" :depends-on ("core-entity" 
                                                        "thing"))
                         (:file "music" :depends-on ("core-entity" 
                                                     "sound"))
                         (:file "particle" :depends-on ("core-entity"))
                         (:file "person" :depends-on ("core-entity"
                                                      "creature"
                                                      "thing"))
                         (:file "place" :depends-on ("core-entity"))
                         (:file "sound" :depends-on ("core-entity"))
                         (:file "thing" :depends-on ("core-entity"))
                         (:file "utterance" :depends-on ("core-entity"))))
   (:module "interfaces"
            :depends-on ("turtar" "entities")
            :components ((:file "cluster-raw-peer-ix")
                         (:file "player-packet-ix" :depends-on ("cluster-raw-peer-ix"))
                         (:file "player-stream-ix" :depends-on ("cluster-raw-peer-ix"
                                                                "player-packet-ix"))
                         (:file "player-unreal-stream-ix" :depends-on ("cluster-raw-peer-ix"
                                                                       "player-stream-ix"))
                         (:file "player-websocket-ix" :depends-on ("cluster-raw-peer-ix"))
                         (:file "sql-persistence-mysql-ix" :depends-on ("cluster-raw-peer-ix"
                                                                        "sql-persistence-peer-ix"))
                         (:file "sql-persistence-peer-ix" :depends-on ("cluster-raw-peer-ix"))
                         (:file "sql-persistence-postgresql-ix" :depends-on ("cluster-raw-peer-ix"
                                                                             "sql-persistence-peer-ix"))
                         (:file "sql-persistence-sqlite-ix" :depends-on ("cluster-raw-peer-ix"
                                                                         "sql-persistence-peer-ix"))))
   (:module "systems"
            :depends-on ("turtar" "entities")
            :components ((:file "physics" :depends-on ("base-system"))
                         (:file "player-movement" :depends-on ("base-system"))
                         (:file "player-sensor" :depends-on ("base-system"))
                         (:file "scripted-movement" :depends-on ("base-system"))
                         (:file "scripted-sensor" :depends-on ("base-system"))
                         (:file "base-system")))))


