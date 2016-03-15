;;; -*- mode: lisp -*-
(defpackage turtar/calendar-printer
  (:use :cl :oliphaunt :turtar/day-night)
  (:export))

(in-package :turtar/calendar-printer)

;;; Copyright © 2016 Bruce-Robert Fenn Pocock
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

(defun print-chœrogryllum-week-planner-for (year month dd)
  (format t "~2%~42:@<Week of ~d ~a, ~4d~>" dd (elt +months+ month) year)
  
  (loop for dow from 0 upto 8
        for d = (1+ (mod (1- (+ dd dow)) 30))
        for m = (mod (+ month (floor (1- (+ dd dow)) 30)) 30)
        do (format t "~2%  ~13<~a~>, ~d ~a, ~d" (elt +days-of-week+ dow)
                   d (elt +months+ m) year)
           
        do (loop for (label offset) in +named-times+
                 for then = (round (+ (find-day-start year m d)
                                      (/ (* 60 60 (* (round offset) 1/18 24))
                                         +chœrogryllum-earth-ratio+)))
                 do (format t "~%~12t~15<~a ~>~{~2,'0d:~2,'0d~}  —  ~a"
                            label
                            (multiple-value-bind (hour sub-hour)
                                (floor offset)
                              (list hour (* sub-hour 60))) 
                            (local-time:format-timestring
                             nil (local-time:universal-to-timestamp then)
                             :format '((:hour 2) #\: (:min 2) #\comma #\space
                                       :short-weekday #\comma #\space
                                       :day #\space :long-month #\comma #\space :year)))))
  
  (format t "~2%The week of ~d ~a, ~d, on Chœrogryllum is related to the Earth dates ranging from "
          dd (elt +months+ month) year)
  (let ((start (local-time:universal-to-timestamp (find-day-start year month dd)))
        (end (local-time:universal-to-timestamp (find-day-start year month (+ 9 dd)))))
    (let ((start-week-day (local-time:format-timestring nil start :format '(:long-weekday)))
          (end-week-day (local-time:format-timestring nil end :format '(:long-weekday)))
          (start-date (local-time:format-timestring  nil start :format '(:day)))
          (end-date (local-time:format-timestring  nil end :format '(:day)))
          (start-month (local-time:format-timestring  nil start :format '(:long-month)))
          (end-month (local-time:format-timestring  nil end :format '(:long-month)))
          (start-year (local-time:format-timestring  nil start :format '(:year)))
          (end-year (local-time:format-timestring  nil end :format '(:year))))
      (cond
        ((equal start-month end-month) 
         (format t "~a ~d - ~a ~d ~a, ~d"
                 start-week-day start-date
                 end-week-day end-date 
                 start-month start-year))
        ((equal start-year end-year)
         (format t "~a, ~d ~a - ~a, ~d ~a, ~d"
                 start-week-day start-date start-month
                 end-week-day end-date end-month
                 start-year))
        (t (format t "~a, ~d ~a, ~d - ~a, ~d ~a, ~d"
                   start-week-day start-date start-month start-year
                   end-week-day end-date  end-month end-year))))))

(defun print-chœrogryllum-week-planner () 
  (multiple-value-bind (seconds minutes hours date month year week-day day-of-year)
      (decode-chœrogryllum-time)
    (declare (ignore seconds minutes hours date month))
    (let ((day-starting-week (- day-of-year week-day)))
      (print-chœrogryllum-week-planner-for year 
                                           (1+ (floor day-starting-week 30)) 
                                           (1+ (mod day-starting-week 30))))))

(defun print-one-week-calendar (&optional (start-time (get-universal-time)))
  (loop for i from 0 upto (* 24 7)
        do (multiple-value-bind (seconds minutes hours date month year week-day)
               (decode-chœrogryllum-time (+ start-time (* 60 60 i))) 
             (sb-int:format-universal-time t (+ start-time (* 60 60 i)))
             (format t " ~40t ")
             (format-chœrogryllum-time seconds minutes hours date month year week-day))))

(defun print-calendar-lead-in (day-of-week-of-1st-of-month &key (extra-days-p t))
  (unless (zerop day-of-week-of-1st-of-month)
    (loop for day from (- 30 (1- (mod day-of-week-of-1st-of-month 9))) upto 30 
          do (if extra-days-p
                 (format t "[~2,' d] " day)
                 (princ "     ")))))

(defun print-calendar-actual-days (day-of-week-of-1st-of-month highlight-date)
  (loop for day from 1 upto 30
        for weekday = (mod (+ day-of-week-of-1st-of-month day) 9)
        
        do (if (= day (or highlight-date 0)) 
               (format t "(~2,' d) " day) 
               (format t " ~2,' d  " day))
           
        when (zerop weekday) do (terpri)))

(defun print-calendar-trailer (day-of-week-of-1st-of-month &key (extra-days-p t))
  (unless (= 6 day-of-week-of-1st-of-month)
    (loop for day from 1 upto (- 9 (mod (+ 3 day-of-week-of-1st-of-month) 9))
          do (if extra-days-p
                 (format t "[~2,' d] " day)
                 (princ "     ")))))



(defun print-calendar-month-header (month year &key (yearp t))
  (if yearp
      (format t "~2%~35,,,'-:@< ~:@r. ~a ~> ~4d" month (elt +months+ month) year)
      (format t "~2%~40,,,'-<~:@r. ~a  ~;~>" month (elt +months+ month))))

(defun print-days-of-week-header ()
  (format t "~2%~{~a ~}" (subseq +dow+ 0 9)))

(defun print-chœrogryllum-monthly-calendar (year month &key ((:highlight-day dd))
                                                            (yearp t) (extra-days-p t))
  (print-calendar-month-header month year :yearp yearp)
  (print-days-of-week-header)
  (multiple-value-bind (_seconds _minutes _hours
                        _date _month _year day-of-week-of-1st-of-month)
      (decode-chœrogryllum-time (find-day-start year month 1))
    (declare (ignore _seconds _minutes _hours _date _month _year))
    (terpri) 
    (print-calendar-lead-in day-of-week-of-1st-of-month :extra-days-p extra-days-p)
    (print-calendar-actual-days day-of-week-of-1st-of-month dd)
    (print-calendar-trailer day-of-week-of-1st-of-month :extra-days-p extra-days-p)) 
  (finish-output))



(defun print-chœrogryllum-annual-calendar (year)
  (multiple-value-bind (seconds minutes h today this-month this-year)
      (decode-chœrogryllum-time)
    (declare (ignore seconds minutes h))
    (loop for month from 1 upto (months-in-year year) by 2
          for left = (with-output-to-string (*standard-output*)
                       (print-chœrogryllum-monthly-calendar year month
                                                            :highlight-day (if (and (= year this-year)
                                                                                    (= month this-month))
                                                                               today)
                                                            :yearp nil :extra-days-p nil))
          for right = (with-output-to-string (*standard-output*)
                        (print-chœrogryllum-monthly-calendar year (1+ month)
                                                             :yearp nil :extra-days-p nil
                                                             :highlight-day (if (and (= year this-year)
                                                                                     (= month this-month))
                                                                                today)))
          do (loop for left-column in (split-sequence #\newline left)
                   for right-column in (split-sequence #\newline right)
                   do (format t "~&~40<~a~;~>   ~a" left-column right-column)))))



(defun print-chœrogryllum-calendar-for (year &optional month dd)
  (if month
      (print-chœrogryllum-monthly-calendar year month :highlight-day dd)
      (print-chœrogryllum-annual-calendar year)))

(defun print-chœrogryllum-calendar ()
  (multiple-value-bind (seconds minutes hours days month year day-of-week)
      (decode-chœrogryllum-time)
    (declare (ignore seconds minutes hours day-of-week))
    (print-chœrogryllum-calendar-for year month days)))

(defun clock ()
  (loop
     (format-current-chœrogryllum-time)
     (sleep 5)))
