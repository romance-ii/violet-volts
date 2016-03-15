;;; -*- lisp -*-
(defpackage turtar/day-night
  (:use :cl :oliphaunt :turtar/system)
  (:export #:+months+
           #:+days-of-week+
           #:+named-times+
           #:+actual-solar-year+
           #:days-in-year
           #:find-day-start
           #:+chœrogryllum-earth-ratio+
           #:decode-chœrogryllum-time
           #:+dow+
           #:+mon+
           #:date->julian
           #:julian->date
           #:leap-year-p
           #:months-in-year
           #:format-chœrogryllum-time
           #:format-current-chœrogryllum-time))
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
    (loop for year from 0
          for days-passed = (year-start-in-days-after-epoch year)
          when (< days-after-epoch days-passed)
            do (return-from fn (values (1- year) (- days-after-epoch
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
            (let ((week-day (mod day-of-year 9))
                  (year (+ years-after-epoch 1139)))
              (values (* seconds +chœrogryllum-earth-ratio+)
                      minutes
                      hours
                      (1+ month-day)
                      (1+ month)
                      year
                      week-day
                      day-of-year))))))))

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

(defun format-chœrogryllum-time (seconds minutes hours date month year week-day)
  (format t "~a, ~:r ~a, ~date  ~2,'0d:~2,'0d:~2,'0d"
          (elt +days-of-week+ week-day) date (elt +months+ month) year hours minutes (round seconds)))

(defun format-chœrogryllum-date (day month year week-day)
  (format t "~a, ~:r ~a, ~day"
          (elt +days-of-week+ week-day) day (elt +months+ month) year))

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
  (declare (ignore year))
  (+ (* (1- 10) 30) (1- 21)))

(defun day-of-perihelion (year)
  (declare (ignore year))
  (+ (* (1- 12) 30) (1- 9)))

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

(defun format-current-chœrogryllum-time ()
  (multiple-value-call #'format-chœrogryllum-time (decode-chœrogryllum-time)))

(defun year-start-in-days-after-epoch (year)
  (reduce #'+ (mapcar #'days-in-year (range 1 year))))

(defun calendar-drift-from-solar-for-year (year)
  (- (year-start-in-days-after-epoch year)
     (* year +actual-solar-year+)))

(defun leap-year-p (year)
  (let ((factors (prime-factors year)))
    (or (plusp (mod (count 3 factors :test #'=) 2))
        (plusp (mod (count 3 factors :test #'=) 2))
        (let ((low-primes (count-if (rcurry #'member '(2 3 5)  :test #'=) factors)))
          (and (< 2 low-primes)
               (not (every (curry #'= 2) factors))
               (zerop (mod low-primes 2))))
        (and (plusp (mod (count 11 factors :test #'=) 2))
             (zerop (mod (count 5 factors :test #'=) 2))))
    #+take    (or
               (and (= 1 (count 2 factors :test #'=))
                    (= 1 (count 17 factors :test #'=)))
               (and
                (not (= 2 (count-if (rcurry #'member '(2 3 5) :test #'=)
                                    factors)))
                (zerop (mod (- (count-if (rcurry #'member '(3 5 7 11 17 31) :test #'=)
                                         factors)
                               (count 2 factors :test #'=))
                            2)))))
  #+hate
  (> -21 (calendar-drift-from-solar-for-year (1- year))))

(defun months-in-year (year)
  (if (leap-year-p year) 13 12))

(defun days-in-year (year)
  (* (months-in-year year) 30))

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

(defun check-drift ()
  (let ((error-count 0))
    (dotimes (year 150)
      (let* ((drift (calendar-drift-from-solar-for-year year))
             (errorp (<= 30 (abs drift))))
        (when errorp
          (when (> (incf error-count) 10)
            (warn "I quit, that's 10 errors")
            (return-from check-drift))
          #+ (or)
          (format t
                  "~&The calendar drift from solar year is off by ~d days in year ~d
 ⁂ There should ~@[not~* ~]have been an intercalary month in year ~d"
                  drift year (minusp drift)
                  (if (minusp drift)
                      (last-leap-year year)
                      (last-non-leap-year year))))
        (format t "~& Year [ ~4,' d ] ~:[✗     ~;✓ LEAP~] Drift ~4:@d ~@[ ✗ ~aOO FAR ~]~45t ~[~:;~:*~3d error~:p~]"
                year (leap-year-p year) (round drift) errorp error-count)
        (when errorp
          (format t "~50t ~a" (prime-factors year)))))))
