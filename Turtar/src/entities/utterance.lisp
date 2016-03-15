;;; -*- lisp -*-
(defpackage turtar/utterance
  (:use :cl :oliphaunt :turtar/entity)
  (:export #:utterance #:language-listener #:language-speaker))
(in-package :turtar/utterance)

;;; Utterance
;;; 
;;; Part of Turtar
;;; 
;;; Copyright © 2016, Bruce-Robert Fenn Pocock
;;;
;;; This program is free  software: you can redistribute it and/or  modify it under the terms of  the GNU Affero General
;;; Public License as  published by the Free Software Foundation,  either version 3 of the License,  or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in  the hope that it will be useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of  MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU  Affero General Public  License for
;;; more details.
;;; 
;;; You should  have received a  copy of  the GNU Affero  General Public License  along with  this program. If  not, see
;;; <http://www.gnu.org/licenses/>.

(defclass utterance (sound)
  ((language :initarg :language :reader utterange-language
             :initform nil)
   (phrase :initarg :phrase :reader utterance-phrase)
   (speaker :initarg :speaker :reader utterance-speaker)
   (volume :initarg :volume :reader utterance-volume
           :initform 1)
   (origin :initarg :origin :reader utterance-origin)
   (duration :initarg :duration :reader utterance-duration)
   (when-spoken :initarg :when-spoken :reader utterance-spoken)
   (medium :initarg :medium :reader utterance-medium
           :initform :aloud)))

(defclass language-speaker (thing)
  ((caption-colors :initarg :caption-colors :reader language-speaker-caption-colors
                   :initform '(:black :white))
   (caption-style :initarg :caption-style :reader language-speaker-caption-style
                  :initform :normal)))

(defclass language-listener (thing)
  ((understands :initarg :understands :reader language-listener-understands
                :initform '(:en-us))
   (media :initarg :media :reader language-listener-media
          :initform nil)))

(defun language-listener-understands-p (listener language)
  (or (null language)
      (member language (language-listener-understands listener))))

(defun listener-close-enough-to-hear-p (listener utterance)
  (< (turtar/geometry:distance-between listener (utterance-speaker utterance))
     (* (utterance-volume utterance) 10))) ; magic number for propagation volume

(defun listener-can-hear-medium-p (listener medium)
  (member medium (language-listener-media listener)))

(defun listener-can-hear-utterance-p (listener utterance)
  (if (eql :aloud (utterance-medium utterance))
      (listener-close-enough-to-hear-p listener utterance)
      (listener-can-hear-medium-p listener (utterance-medium utterance))))

