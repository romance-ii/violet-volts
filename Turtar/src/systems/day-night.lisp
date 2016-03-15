;;; -*- lisp -*-
(
 defpackage turtar/day-night
  (:use :cl :oliphaunt :turtar/system)
  (:export #:+months+
           #:+days-of-week+
           #:+named-times+
           #:+actual-solar-year+
           #:days-in-year
           #:find-day-start
           #:date->day-of-week
           #:+chœrogryllum-earth-ratio+
           #:chœrogryllum-week-day
           #:chœrogryllum-date
           #:chœrogryllum-month
           #:chœrogryllum-year
           #:chœrogryllum-hour
           #:chœrogryllum-minute
           #:chœrogryllum-second
           #:decode-chœrogryllum-time
           #:+dow+
           #:+mon+
           #:date->julian
           #:julian->date
           #:leap-year-p
           #:months-in-year
           #:day-of-vernal-equinox
           #:day-of-autumnal-equinox
           #:day-of-summer-solstice
           #:day-of-winter-solstice
           #:format-chœrogryllum-time
           #:format-chœrogryllum-date
           #:format-chœrogryllum-date&time
           #:format-current-chœrogryllum-time
           #:year-start-in-days-after-epoch))
(in-package :turtar/day-night)

;;; Day-Night
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

(defclass day-night-system (proc-system)
  ())

(defun day-night-update ()
  ())

(defmethod turtar:hook-world-bootstrap progn (world)
  (make-instance 'day-night-system
                 :operator #'day-night-update
                 :selector nil
                 :reaction (scheduled-reaction :interval 30)))

(define-constant +chœrogryllum-earth-ratio+ 9/7)

(defun epoch-tz-offset ()
  (* 19 60 60))

(defun earth-day-length ()
  (* 60 60 24))

(defun universal-time->days+seconds (universal-time)
  (floor (- universal-time (epoch-tz-offset))
         (/ (earth-day-length) +chœrogryllum-earth-ratio+)))

(defun days-after-epoch->years-after-epoch+day-of-year (days-after-epoch)
  (block fn
    ;;; We're sure it'll be after this date, at least.
    (loop for year from (min 1 (truncate days-after-epoch 390))
       for days-passed = (year-start-in-days-after-epoch year)
       then (+ days-passed (days-in-year year))
       when (< days-after-epoch days-passed)
       do (return-from fn (values (1- year)
                                  (- days-after-epoch
                                     (- days-passed (days-in-year year))))))))

(defun decode-chœrogryllum-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (days-after-epoch day-seconds)
      (universal-time->days+seconds universal-time)
    (multiple-value-bind (day-minutes seconds)
        (floor day-seconds (/ 60 18/24 +chœrogryllum-earth-ratio+))
      (multiple-value-bind (hours minutes)
          (floor day-minutes 60)
        (multiple-value-bind (years-after-epoch day-of-year)
            (days-after-epoch->years-after-epoch+day-of-year days-after-epoch)
          (multiple-value-bind (month month-day)
              (floor day-of-year 30)
            (let ((week-day (mod days-after-epoch 9))
                  (year years-after-epoch))
              (values hours
                      minutes
                      (round seconds +chœrogryllum-earth-ratio+)
                      (1+ month-day)
                      (1+ month)
                      year
                      week-day
                      day-of-year
                      days-after-epoch))))))))

(defun date->day-of-week (year month day) 
  (mod (days-after-epoch year month day) 9))

(define-constant +days-of-week+ '("Lighteningday" "Spotsday" "Starsday" "Notesday" "Sparkleday"
                                  "Moosday" "Heartsday" "Floralday" "Blanksday" "—")
  :test #'equalp)

(define-constant +dow+ '("Ltn." "Spt." "Str." "Not." "Spk." "Moo." "Hrt." "Flr." "Bnk." "—")
  :test #'equalp)

(define-constant +months+ '("—"
                            "Sirenia" "Dugon" "Inunguis" "Manatus"
                            "Hydrodamalis" "Senecalensis" "Pygmaeus" "Loxodonta"
                            "Elephas"  "Procavia" "Dendrohyrax" "Tethytheria"
                            "Tartarus")
  :test #'equalp)

(define-constant +mon+ '("—" "Sir." "Dug." "Inu." "Man."
                         "Hyd." "Sen." "Pyg." "Lox."
                         "Ele."  "Pro." "Den." "Teth."
                         "Tar.")
  :test #'equalp)

(defun format-chœrogryllum-date&time (s
                                      seconds minutes hours
                                      date month year week-day
                                      &rest _)
  (declare (ignore _))
  (format s "~a ~a"
          (format-chœrogryllum-date nil year month date week-day)
          (format-chœrogryllum-time nil hours minutes seconds))) 

(defun format-chœrogryllum-time (s seconds minutes hours)
  (format s "~2,'0d:~2,'0d:~2,'0d"
          hours minutes (round seconds)))

(defun format-chœrogryllum-date (s year month day week-day)
  (format s "~a, ~:r ~a, ~d"
          (elt +days-of-week+ week-day) day (elt +months+ month) 
          year))

(define-constant +named-times+ (list '("Midnight" 0)
                                     '("Breakfast" 6)
                                     '("Brunch" 8)
                                     '("Noon" 9)
                                     '("Lunch" 11)
                                     '("Suppertime" 13)
                                     '("Tea-time" 14)
                                     '("Dinner" 15)
                                     (list "Day ends" (+ 17 59/60)))
  :test #'equalp)

(define-constant +orbital-eccentricity-of-chœrogryllum+ .01457
  :test #'=)

(defun day-of-winter-solstice (year)
  (floor (+ (calendar-drift-from-solar-for-year year)
            (+ (* (1- 10) 30) (1- 21)))))

(defun day-of-vernal-equinox (year)
  (floor (- (day-of-winter-solstice year) (* 3 (truncate +actual-solar-year+ 4)))))

(defun day-of-summer-solstice (year) 
  (floor (- (day-of-winter-solstice year) (truncate +actual-solar-year+ 2))))

(defun day-of-autumnal-equinox (year)
  (floor (- (day-of-winter-solstice year) (truncate +actual-solar-year+ 4))))

(defun day-of-perihelion (year)
  (floor (mod (+ (calendar-drift-from-solar-for-year year)
                 (* (1- 12) 30) (1- 9))
              (days-in-year year))))

(defun day-of-epihelion (year)
  (floor (mod (+ (day-of-perihelion year) (truncate +actual-solar-year+ 2))
              (days-in-year year))))

(defconstant +actual-solar-year+ 368.917654
  "The time for Chœrogryllum to orbit Tiberius once, in days.")

(defconstant +day-fraction+ (/ (* 2 pi) +actual-solar-year+)
  "The degrees traveled per day")

(defun angular-position-of-orbit (day-of-year year)
  (* +day-fraction+ (mod (- (day-of-winter-solstice year) day-of-year) 360)))

(defun angular-position-with-eccentricity-correction (day-of-year year)
  (+ (angular-position-of-orbit day-of-year year)
     (* 2
        +orbital-eccentricity-of-chœrogryllum+
        (sin (* +day-fraction+ (- day-of-year (day-of-perihelion year)))))))

(defconstant +obliquity-of-orbit-of-chœrogryllum+ 14.5869469)

(defun difference-of-angular-position-on-equatorial-plane (day-of-year year)
  (/ (- (angular-position-of-orbit day-of-year year)
        (atan (/ (tan (angular-position-with-eccentricity-correction day-of-year year))
                 (cos +obliquity-of-orbit-of-chœrogryllum+))))
     pi))

(defun equation-of-time (day-of-year year)
  (multiple-value-bind (_ fraction)
      (round (difference-of-angular-position-on-equatorial-plane day-of-year year))
    (declare (ignore _))
    (* 9 60 60 fraction)))

(defun solar-declination (day-of-year year)
  (- (asin (* (sin +obliquity-of-orbit-of-chœrogryllum+)
              (cos (angular-position-with-eccentricity-correction day-of-year year))))))

(defun sunrise-sunset (day-of-year)
  )


(defun days-after-epoch (year month day)
  (+ (year-start-in-days-after-epoch year)
     (* 30 (1- month))
     (1- day)))

(defun find-day-start (year month day)
  (let* ((days-after-epoch (days-after-epoch year month day))
         (seconds-after-epoch (* days-after-epoch (/ (* 60 60 24)
                                                     +chœrogryllum-earth-ratio+))))
    ;; (sb-int:format-universal-time t (round seconds-after-epoch))
    (round seconds-after-epoch)))

(defun format-current-chœrogryllum-time (&optional (s t))
  (multiple-value-call (curry #'format-chœrogryllum-date&time s)
    (decode-chœrogryllum-time)))

(defun chœrogryllum-week-day (&optional (universal-time (get-universal-time)))
  (nth 6 (multiple-value-list (decode-chœrogryllum-time universal-time))))
(defun chœrogryllum-year (&optional (universal-time (get-universal-time)))
  (nth 5 (multiple-value-list (decode-chœrogryllum-time universal-time))))
(defun chœrogryllum-month (&optional (universal-time (get-universal-time)))
  (nth 4 (multiple-value-list (decode-chœrogryllum-time universal-time))))
(defun chœrogryllum-date (&optional (universal-time (get-universal-time)))
  (nth 3 (multiple-value-list (decode-chœrogryllum-time universal-time))))
(defun chœrogryllum-hour (&optional (universal-time (get-universal-time)))
  (nth 2 (multiple-value-list (decode-chœrogryllum-time universal-time))))
(defun chœrogryllum-minute (&optional (universal-time (get-universal-time)))
  (nth 1 (multiple-value-list (decode-chœrogryllum-time universal-time))))
(defun chœrogryllum-second (&optional (universal-time (get-universal-time)))
  (nth 0 (multiple-value-list (decode-chœrogryllum-time universal-time))))

(defun year-start-in-days-after-epoch (year)
  (cond
    ((= 1 year) 0)
    ((<= 2 year 1299)
     (+ (days-in-year (1- year))
        (year-start-in-days-after-epoch (1- year))))
    (t (error "Unsupported year"))))

(defun calendar-drift-from-solar-for-year (year)
  (- (year-start-in-days-after-epoch year)
     (* (1- year) +actual-solar-year+)))

(defun leap-year-p (year) 
  (if (zerop year) 
      nil
      (or (and (zerop (rem year 3))
               (or (and (not (zerop (rem year (* 7 3))))
                        (not (zerop (rem year (* 3 3 3 2))))
                        (not (zerop (rem year (* 3 3 3 3))))
                        (not (zerop (rem year (* 47 3))))
                        (not (zerop (rem year (* 19 19 3))))
                        (not (zerop (rem year (* 13 5 3))))
                        (not (zerop (rem year (* 37 3 2))))
                        (not (zerop (rem year (* 37 5 3))))
                        (not (zerop (rem year (* 37 13 3))))
                        (not (zerop (rem year (* 29 5 3 3))))
                        (not (zerop (rem year (* 37 3 3))))
                        (not (zerop (rem year (* 71 3 3))))
                        (not (zerop (rem year (* 61 5 3))))
                        (not (zerop (rem year (* 79 3 2))))
                        (not (zerop (rem year (* 59 3 2 2 2)))) 
                        (not (zerop (rem year (* 17 3 3 2))))
                        (not (zerop (rem year (* 67 3 2 2))))
                        (not (zerop (rem year (* 109 3 2 2))))
                        (not (zerop (rem year (* 167 3))))
                        (not (zerop (rem year (* 139 3 2))))
                        (not (zerop (rem year (* 199 3 2))))
                        (not (zerop (rem year (* 241 3))))
                        (not (zerop (rem year (* 379 3))))
                        (not (zerop (rem year (* 389 3))))
                        (not (zerop (rem year (* 5 5 5 3 2)))))
                   (zerop (rem year (* 13 5 3 2 2)))
                   (zerop (rem year (* 7 3 2)))
                   (zerop (rem year (* 7 3 3)))
                   (zerop (rem year (* 7 7 3)))
                   (zerop (rem year (* 23 3 3 2)))
                   
                   (zerop (rem year (* 3 3 3 2 2)))
                   (zerop (rem year (* 3 7 11)))
                   (zerop (rem year (* 3 3 3 3 11)))
                   (zerop (rem year (* 5 3 3 3 3 2)))
                   (zerop (rem year (* 3 7 13)))
                   (zerop (rem year (* 17 3 3 2 2)))
                   (zerop (rem year (* 47 3 2)))
                   (zerop (rem year (* 47 3 3)))
                   (zerop (rem year (* 19 7 3)))
                   (zerop (rem year (* 79 3 2 2))))
               (or (not (zerop (rem year (* 3 3 7 11))))
                   (zerop (rem year (* 2 3 3 7 11))))
               (not (zerop (rem year (* 3 3 3 7 5))))
               (not (zerop (rem year (* 3 3 3 3 3 2 2)))))
          
          (and (zerop (rem year (* 11 11 2 2)))
               (not (zerop (rem year (* 11 11 2 2 2)))))
          (zerop (rem year (* 73 5 2)))
          (and (zerop (rem year (* 17 7 5)))
               (not (zerop (rem year (* 17 7 5 2)))))
          (zerop (rem year (* 181 5)))
          (and (zerop (rem year (* 37 19)))
               (not (zerop (rem year (* 37 19 2))))) 
          (zerop (rem year (* 17 59)))
          (zerop (rem year (* 353 2)))
          (zerop (rem year (* 61 2 2 2)))
          (zerop (rem year (* 5 5 5 2 2 2)))
          (zerop (rem year (* 557 2)))
          (zerop (rem year (* 641 2)))
          (zerop (rem year 653))
          (zerop (rem year 919))
          (zerop (rem year 1171))
          (zerop (rem year 1423))
          (zerop (rem year 1447))))
  #+hate
  (> -21 (calendar-drift-from-solar-for-year (1- year))))

(defun months-in-year (year)
  (if (leap-year-p year) 13 12))

(defun days-in-year (year)
  (cond ((zerop year) 0)
        ((<= 1 year 1299)
         (* (months-in-year year) 30))
        (t (error "Unsupported year"))))


(defun last-non-leap-year (year)
  (if (leap-year-p year)
      (last-non-leap-year (1- year))
      year))

(defun last-leap-year (year)
  (if (leap-year-p year)
      year
      (last-non-leap-year (1- year))))

(defun prime-factors (figure &optional siblings)
  (when (> figure 1)
    (if (evenp figure)
        (prime-factors (/ figure 2) (cons 2 siblings))
        (loop with max-factor = (isqrt figure)
           for factor? = 3 then (1+ factor?)
           for (quotient remainder) = (multiple-value-list (truncate figure factor?))
           do (cond ((> factor? max-factor)
                     (return (cons figure siblings)))
                    ((zerop remainder)
                     (return (prime-factors quotient
                                            (cons factor? siblings)))))))))

(dotimes (year 1300)
  (when (plusp year)
    (let* ((drift (calendar-drift-from-solar-for-year year)))
      (when (<= 30 (abs drift))
        #+nil
        (format t "~& Year [ ~4,' d ] ~:[✗     ~;✓ LEAP~] Drift ~4:@d  ✗ TOO FAR"
                year (leap-year-p year) (round drift))
        (error
         "~&The calendar drift from solar year is off by ~d days in year ~d
 ⁂ There should ~@[not~* ~]have been an intercalary month in year ~d"
         drift year (minusp drift)
         (if (minusp drift)
             (last-leap-year year)
             (last-non-leap-year year)))))))

(defun date->julian (year month day)
  (- (days-after-epoch year month day)
     (year-start-in-days-after-epoch year)))

(defun julian->date (year julian-day)
  (multiple-value-bind (_s _m _h day month year week-day)
      (decode-chœrogryllum-time 
       (/ (* (+ (year-start-in-days-after-epoch year)
                julian-day)
             24 60 60) 
          +chœrogryllum-earth-ratio+))
    (declare (ignore _s _m _h))
    (values year month day week-day)))

(assert (= 0 (year-start-in-days-after-epoch 1)))
(assert (= (days-after-epoch 1285 1 1)
           (year-start-in-days-after-epoch 1285)))
(assert (equalp (multiple-value-list 
                 (days-after-epoch->years-after-epoch+day-of-year
                  (days-after-epoch 1285 1 1)))
                (list 1285 0)))
(assert (leap-year-p 1284))
(assert (not (leap-year-p 1285)))
(assert (= 390 (days-in-year 1284)))
(assert (= 360 (days-in-year 1285)))
(assert (= (days-after-epoch 1285 1 1)
           (+ 30 (days-after-epoch 1284 13 1))) ())


