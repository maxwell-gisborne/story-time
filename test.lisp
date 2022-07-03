(defpackage :story-time
  (:use :cl))

(in-package :story-time)

;; --- The Printing Stuff ---
(defun print-line (&optional (string ""))
  (format T "~%~a" string))

(defun print-chapter-heading (string )
  (let ((line (make-string (length string) :initial-element #\-)))
    (print-line)
    (print-line (string-capitalize string))
    (print-line line)))


;; --- Time and dates ---


(defun overflow-and-remainder (a b factor)
  (multiple-value-bind (overflow remainder) (floor b factor)
    (cons (+ overflow a) remainder)))

(defun make-time (&key (year 0) (month 0) (day 0) (hour 0) (minute 0) (second 0))
  (let* ((minute-second (overflow-and-remainder minute second 60))
	 (hour-minute (overflow-and-remainder hour (car minute-second) 60))
	 (day-hour (overflow-and-remainder day (car hour-minute) 24))
	 (month-day (overflow-and-remainder month (car day-hour) 30))
	 (year-month (overflow-and-remainder year (car month-day) 12)))
    (list :year (car year-month)
	  :month (cdr year-month)
	  :day (cdr month-day)
	  :hour (cdr day-hour)
	  :minute (cdr hour-minute)
	  :second (cdr minute-second))))

(defparameter *time-now* (make-time :year 0
				    :month 5
				    :day 13
				    :hour 15
				    :second 0))

(defun time-add (time1 time2)
  (make-time
   :year (+ (getf time1 :year) (getf time2 :year))
   :month (+ (getf time1 :month) (getf time2 :month))
   :day (+ (getf time1 :day) (getf time2 :day))
   :hour (+ (getf time1 :hour) (getf time2 :hour))
   :minute (+ (getf time1 :minute) (getf time2 :minute))
   :second (+ (getf time1 :second) (getf time2 :second))))

(defun time-sub (time1 time2)
  (make-time
   :year (- (getf time1 :year) (getf time2 :year))
   :month (- (getf time1 :month) (getf time2 :month))
   :day (- (getf time1 :day) (getf time2 :day))
   :hour (- (getf time1 :hour) (getf time2 :hour))
   :minute (- (getf time1 :minute) (getf time2 :minute))
   :second (- (getf time1 :second) (getf time2 :second))))


;; --- The Place and moveing stuff 
(defun make-place (&key (name :Anonymouse)
		     (description :Non-descript)
		        (modes NIL))

  (list :name name :modes modes :description description ))

'(defun make-mode (destination time &key (description :Non-descript))
  (list  :destination  destination
	 :description description
	 :time time
	 :use ( lambda ())))

;; --user stuff --

(defun make-user (&key (health '())
		       (knowledge '())
		       (skills '())
		       (clothing '())
		       (equipment '()))
  (list :health health :knowledge knowledge :skills skills :equiptment equipment
	:clothing clothing))

(defun describe-health (user)
  (print-line "describing health")
  (let ((health (getf user :health)))
    (if (equal health NIL)
	(print-line "good and nice and healthy :^)")
	(progn
	  (if (member :headake health)
	      (print-line "I have a huge headake :("))
	  (if (member :open-woond health)
	      (print-line "I have an open woond"))
	  (if (member :dehydrated health)
	      (print-line "I am dehidgrated"))))))

(defparameter *the-user* (make-user :health '(:headake :dehydrated)
				    :skills '()
				    :equipment '()
				    :clothing '() ))

;; So we have seanse, descriptions of seinces, they take place in locations, 

(defparameter The-Void (make-place :name "The Void"
		 		   :description "Iternal empty Nothingness"))

(defvar *current-location* The-Void)
(defun rest-location () (setf *current-location* initial-location))

(defparameter exiter-st-david (make-place :name "exiter St David"))

(defun describe-place (place)
  (let ((description (getf place :description)))
    (if (equal description :Non-descript)
	(print-line "This place is remarkably non-descript")
	(print-line description))))

(defun describe-this-place ()
  (describe-place *current-location*))

(defun script ()
  (progn
    (print-chapter-heading "wellcome to ~story~ time")
    (setf *current-location* initial-location)
    ))

(script)
