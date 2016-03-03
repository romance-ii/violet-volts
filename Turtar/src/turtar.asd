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
  ((:module "entities"
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
            :depends-on ("entities")
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
            :depends-on ("entities")
            :components ((:file "physics")
                         (:file "player-movement")
                         (:file "player-sensor")
                         (:file "scripted-movement")
                         (:file "scripted-sensor")))))

(defpackage turtar (:use :cl) (:export #:turtar))

