(defpackage :violet-volts
  (:use :cl :jscl/ffi)
  (:export #:-then->))
(defpackage :webrepl
  (:use :cl :jscl/ffi)
  (:export #:start-repl))
(defpackage :webinspect
  (:use :cl :jscl/ffi)
  (:export #:inspect))
(defpackage :webdebug
  (:use :cl :jscl/ffi)
  (:export #:debugger #:debugger-js))
(defpackage :parrot
  (:use :cl :jscl/ffi)
  (:export #:squawk #:peep #:feather))
(defpackage :babylon
  (:use :cl :jscl/ffi))
(defpackage :device.ambient-light
  (:use :cl :jscl/ffi))
(defpackage :ux.overlay
  (:use :cl :jscl/ffi))
(defpackage :ux.parrot
  (:use :cl :jscl/ffi)
  (:export #:present-parrot))
(defpackage :ux.gossip-mouse
  (:use :cl :jscl/ffi)
  (:export #:present-gossip-mouse))
(defpackage :parrot.new-player
  (:use :cl :jscl/ffi)
  (:export #:new-player))
(defpackage :parrot.buddy-list
  (:use :cl :jscl/ffi)
  (:export #:add-buddy #:remove-buddy #:present-buddy #:present-buddy-list))
(defpackage :parrot.child-account
  (:use :cl :jscl/ffi)
  (:export #:add-child #:present-child #:present-children))
(defpackage :parrot.child-login
  (:use :cl :jscl/ffi)
  (:export #:request-child-login #:parent-validate-child-login))
(defpackage :ux.events
  (:use :cl :jscl/ffi))
(defpackage :ux.make-noise
  (:use :cl :jscl/ffi)
  (:export #:toot))
(defpackage :ux.speak
  (:use :cl :jscl/ffi)
  (:export #:speak))
(defpackage :chatter
  (:use :cl :jscl/ffi)
  (:export #:place-call #:answer-call #:send-sms #:send-mms-photo #:send-mms-audio
           #:send-call-to-voice-mail #:present-sms #:present-mms-photo #:present-mms-audio
           #:present-voice-mail #:present-mobile-conversation #:present-mobile-conversations))
(defpackage :player-input.lipread
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.listen
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.events
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.keyboard
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.touch
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.gamepad
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.tabletpad
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.bluetooth-gamepad
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :player-input.bluetooth-wiimote
  (:use :cl :jscl/ffi)
  (:export #:configure #:start #:stop))
(defpackage :device.vibration
  (:use :cl :jscl/ffi)
  (:export #:vibrate))
(defpackage :device.orientation
  (:use :cl :jscl/ffi))
(defpackage :xhr
  (:use :cl :jscl/ffi)
  (:export #:xhr))
(defpackage :login
  (:use :cl :jscl/ffi)
  (:export #:google-login #:facebook-login))
(defpackage :gossipnet.net
  (:use :cl :jscl/ffi)
  (:export #:start #:stop))
(defpackage :gossipnet.events
  (:use :cl :jscl/ffi))
(defpackage :gossipnet.integrity
  (:use :cl :jscl/ffi))
(defpackage :gossipnet.genesis
  (:use :cl :jscl/ffi))
(defpackage :assets
  (:use :cl :jscl/ffi)
  (:export #:fetch-bytes))
(defpackage :assets.service
  (:use :cl :jscl/ffi))
(defpackage :assets.torrent
  (:use :cl :jscl/ffi))
(defpackage :assets.archives
  (:use :cl :jscl/ffi))
(defpackage :world.events
  (:use :cl :jscl/ffi)
  (:export #:make-event #:dispatch-event))
(defpackage :device.network
  (:use :cl :jscl/ffi))
