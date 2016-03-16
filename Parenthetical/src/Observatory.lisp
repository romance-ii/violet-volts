(defpackage turtar/observatory
  (:use :cl :oliphaunt))
(in-package :turtar/observatory)

(defmacro observation-page ((title) &body body)
  `(cl-who:with-html-output (t :prologue t :indent t)
     (:html :lang "en"
            (:head 
             (:meta :http-equiv "Content-Type" 
                    :content "text/html;charset=utf-8")
             (:title ,title)
             (:link :type "text/css" 
                    :rel "stylesheet"
                    :href "/observatory.css"))
            (:body 
             (:nav :id "header"
                   (:h1 ,title)
                   (:ul :id "menu-bar"
                        (:li (:a :href "/" "Observatory"))))
             ,@body))))

(defmacro html (&body body)
  `(cl-who:with-html-output (t)
     ,@body))

(defun print-chœrogryllum-week-planner-for (year month date)
  (observation-page 
   ((format nil "Weekly Planner: Week of ~d ~a, ~d"
            date (elt turtar/day-night:+months+ month) year)) 
   (:table 
    :class "weekly-planner"
    (:tbody
     (loop for dow from 0 upto 8
        for d = (1+ (mod (1- (+ dd dow)) 30))
        for m = (mod (mod (+ month (floor (1- (+ dd dow)) 30)) 30) (turtar/day-night:months-in-year year))
        for y = (if (>= m month) year (1+ year))
        do (html
            (:tr (:td :colspan 4
                      (:h2
                       (format nil "~2%  ~13<~a~>, ~d ~a, ~d" (elt turtar/day-night:+days-of-week+ dow)
                               d (elt turtar/day-night:+months+ m) y)))))
          
        do (loop for (label offset) in turtar/day-night:+named-times+
              for then = (round (+ (find-day-start y m d)
                                   (/ (* 60 60 (* (round offset) 1/18 24))
                                      turtar/day-night:+chœrogryllum-earth-ratio+)))
              do (html (:tr (:th label)
                            (:td (multiple-value-bind (hour sub-hour)
                                     (floor offset)
                                   (format nil "~2,'0d:~2,'0d" hour (* sub-hour 60))))
                            (:td (local-time:format-timestring
                                  nil (local-time:universal-to-timestamp then)
                                  :format '((:hour 2) #\: (:min 2))))
                            (:td (local-time:format-timestring
                                  nil (local-time:universal-to-timestamp then)
                                  :format '(:day #\space :long-month #\comma #\space :year)))))))))
   (:p 
    (format nil "~2%The week of ~d ~a, ~d, on Chœrogryllum is related to
the Earth dates ranging from "
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
           (format nil "~a ~d - ~a ~d ~a, ~d"
                   start-week-day start-date
                   end-week-day end-date 
                   start-month start-year))
          ((equal start-year end-year)
           (format nil "~a, ~d ~a - ~a, ~d ~a, ~d"
                   start-week-day start-date start-month
                   end-week-day end-date end-month
                   start-year))
          (t (format nil "~a, ~d ~a, ~d - ~a, ~d ~a, ~d"
                     start-week-day start-date start-month start-year
                     end-week-day end-date  end-month end-year))))))))

(defun print-chœrogryllum-week-planner (&optional (week-delta 0))
  (multiple-value-bind (seconds minutes hours date month year week-day day-of-year)
      (decode-chœrogryllum-time)
    (declare (ignore seconds minutes hours date month))
    (let ((day-starting-week (- day-of-year (+ week-day (* 9 week-delta)))))
      (print-chœrogryllum-week-planner-for year 
                                           (1+ (floor day-starting-week 30)) 
                                           (1+ (mod day-starting-week 30))))))

(defun print-one-week-calendar (&optional (start-time (get-universal-time)))
  (loop for i from 0 upto (* 24 7)
     do (multiple-value-bind (seconds minutes hours date month year week-day)
            (decode-chœrogryllum-time (+ start-time (* 60 60 i))) 
          (sb-int:format-universal-time t (+ start-time (* 60 60 i)))
          (format t " ~40t ")
          (format-chœrogryllum-date&time t
                                         seconds minutes hours
                                         date month year week-day))))

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
  (let ((day-of-week-of-1st-of-month (date->day-of-week year month 1)))
    (terpri) 
    (print-calendar-lead-in day-of-week-of-1st-of-month :extra-days-p extra-days-p)
    (print-calendar-actual-days day-of-week-of-1st-of-month dd)
    (print-calendar-trailer day-of-week-of-1st-of-month :extra-days-p extra-days-p)) 
  (finish-output))

(defun print-chœrogryllum-annual-calendar (&optional year)
  (multiple-value-bind (seconds minutes h today this-month this-year)
      (decode-chœrogryllum-time)
    (declare (ignore seconds minutes h))
    (unless year
      (setf year this-year))
    (format t "~80<~;Chœrogryllum Calendar, Year ~d~;~>" year)
    (loop for month from 1 upto 11 by 2
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
                                                                                  (= (1+ month) this-month))
                                                                             today)))
       do (loop for left-column in (split-sequence #\newline left)
             for right-column in (split-sequence #\newline right)
             do (format t "~&~40<~a~;~>   ~a" left-column right-column)))
    (when (leap-year-p year)
      (let ((tartarus (with-output-to-string (*standard-output*)
                        (print-chœrogryllum-monthly-calendar year 13
                                                             :yearp nil :extra-days-p nil
                                                             :highlight-day (if (and (= year this-year)
                                                                                     (= 13 this-month))
                                                                                today)))))
        (dolist (line (split-sequence #\newline tartarus))
          (format t "~&~21t~a" line))))))

(defun print-seasonal-almanack (&key (year (chœrogryllum-year))
                                     (verbose t))
  (format t "~&Almanack of Seasons for the year ~d~%" year)
  
  (loop for (label value)
     in (list (list "Vernal Equinox" (day-of-vernal-equinox year))
              (list "Summer Solstice" (day-of-summer-solstice year))
              (list "Autumnal Equinox" (day-of-autumnal-equinox year))
              (list "Winter Solstice" (day-of-winter-solstice year)))
     do (multiple-value-bind (year month date day)
            (julian->date year value)
          (format t "~%~20<~a:~> ~a"
                  label
                  (format-chœrogryllum-date nil year month date day))
          (when verbose
            (terpri)
            (print-chœrogryllum-calendar-for year month date)
            (terpri)))))

(defun print-tourist-guide ()
  (format t "~&~|~2%This report was generated at:~%~5t")
  (format-current-chœrogryllum-time)
  (format t "~&(On Earth: ~a)" 
          (format-timestring nil (universal-to-timestamp (get-universal-time))
                             :format '(:long-weekday #\Comma #\Space
                                       :day #\Space :long-month #\Comma
                                       #\Space :year)))
  (format t "~4% This month, at a glance:")
  (print-chœrogryllum-calendar)
  (format t "~4%

Planning to visit from Earth?  Here's a time comparison. 

Remember, Chœrogryllum's days last  18 hours*, but our nine-day
week is about the same length as your seven-day week.

Typical meal and event times are listed for your convenience.

*** SORRY ***

Due to problems with the observatory, ephemera such as sunrise 
and eclipses are NOT appearing on this planner currently, nor are 
tides, market day events, holidays, or seasons.

Market Day is Blanksday in all major cities.

")
  (print-chœrogryllum-week-planner)
  (print-chœrogryllum-week-planner -1)
  
  (format t "~2%
*(our hours, minutes, and seconds are slightly longer than yours, too, 
but you probably won't notice the difference.)

")
  (print-seasonal-almanack :verbose nil)
  (terpri)
  (terpri)
  (print-chœrogryllum-annual-calendar))

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
     (terpri)
     (sleep 5))) 


(defun print-pages ()
  )
