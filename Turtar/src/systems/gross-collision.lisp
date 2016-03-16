 ;;; -*- lisp -*-
(defpackage turtar/collision
  (:use :cl :oliphaunt :turtar/thing :turtar/geometry :turtar/system))
(in-package :turtar/collision)
;;;
;;; Fine Collision Detection System
;;;
;;; Part of Turtar
;;;
;;; Copyright  © 2016, Bruce-Robert Fenn Pocock <brpocock@Star-Hope.org>
;;;
;;; This program is free (as in “freedom”) software: you can redistribute it and/or modify it under the terms of the GNU
;;; Affero General Public License, as published by the Free Software Foundation: either version 3 of the License, or (at
;;; your option) any later version which they produce.
;;; 
;;; This program is distributed in  the hope that it will be useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the License text for more details.
;;; 
;;; You should have received a  copy of the License along with this program. If  you obtained this software distribution
;;; in full, it can be found in the file COPYING.AGPL3 at  the top-level of the source code tree, or in the hypertext or
;;; printable  manuals in  the chapter  entitled  Copying. If  you cannot  otherwise find  a  copy of  the license,  see
;;; http://www.gnu.org/licenses/ for  a hypertext  copy; send  email to this  program's maintainer;  write to:  The Free
;;; Software Foundation, Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA; or visit http://www.fsf.org/
;;;
;;; See doc/collision.texi

(defclass collision (proc-system))
