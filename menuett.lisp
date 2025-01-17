(in-package #:menuett)

(eval-when (:load-toplevel :compile-toplevel)
  (defun escaped (string)
    (format nil "~c~a" #\Esc string)))

(alexandria:define-constant +lcd-cls+ (escaped "[2J") :test #'string-equal)
(alexandria:define-constant +lcd-blink-on+ (escaped "[LB") :test #'string-equal)
(alexandria:define-constant +lcd-blink-off+ (escaped "[Lb") :test #'string-equal)
(alexandria:define-constant +lcd-cursor-on+ (escaped "[LC") :test #'string-equal)
(alexandria:define-constant +lcd-cursor-off+ (escaped "[Lc") :test #'string-equal)
(alexandria:define-constant +lcd-cursor-home+ (escaped "[H") :test #'string-equal)
(alexandria:define-constant +lcd-cursor-left+ (escaped "[Ll") :test #'string-equal)
(alexandria:define-constant +lcd-cursor-right+ (escaped "[Lr") :test #'string-equal)
(alexandria:define-constant +lcd-scroll-left+ (escaped "[LL") :test #'string-equal)
(alexandria:define-constant +lcd-scroll-right+ (escaped "[LR") :test #'string-equal)
(alexandria:define-constant +lcd-init+ (escaped "[LI") :test #'string-equal)
(alexandria:define-constant +lcd-kill+ (escaped "[Lk") :test #'string-equal)

(defparameter *up-menu-message* '("EXIT" "Return to" "previous menu" ""))

(defclass display ()
  ((width :accessor width
	  :initarg :width
	  :initform 20)
   (height :accessor height
	   :initarg :height
	   :initform 4)
   (buzzer-actuator :accessor buzzer-actuator
		    :initarg :buzzer-actuator)
   (path :accessor path :initarg :path
	 :initform "/dev/lcd")))


(defmethod write-lcd ((d display) string)
  (with-open-file (s (path d) :direction :output :if-exists :append)
    (write-string string s))
  t)

(defmethod scrollout-right ((d display))
  (loop repeat 12
	for coeff downfrom 1 by 0.2
	do (write-lcd d +lcd-scroll-right+)
	   (sleep (* 0.1 (max 0 coeff))))
  (write-lcd d +lcd-cls+)
  (loop repeat 12 do (write-lcd d +lcd-scroll-left+)))

(defmethod scrollout-left ((d display))
  (loop repeat 12
	for coeff downfrom 1 by 0.2
	do (write-lcd d +lcd-scroll-left+)
	   (sleep (* 0.05 (max 0 coeff))))
  (write-lcd d +lcd-cls+)
  (loop repeat 10 do (write-lcd d +lcd-scroll-right+)))

(defmethod writeout-screen ((d display) text-lines)
  (dolist (line text-lines)
    (write-lcd d line)
    (write-lcd d +lcd-kill+)
    (write-lcd d (format nil "~%"))))

(defmethod swipe-left ((d display) new-lines)
  (swipe-right d new-lines +lcd-scroll-left+))

(defmethod swipe-right ((d display) new-lines &optional (scroll-code +lcd-scroll-right+))
  (write-lcd d +lcd-cursor-home+)
  (unwind-protect
       (loop for pos from 0 to (1- (width d))
	     for coeff downfrom 1 by 0.2
	     do (write-lcd d scroll-code)
		(when (= pos (1- (width d)))
		  (funcall (buzzer-actuator d) t))
		(when (and new-lines (> pos (1- (/ (width d) 2))))
		  (write-lcd d (car new-lines))
		  (setf new-lines (cdr new-lines))
		  (write-lcd d (format nil "~a~%" +lcd-kill+)))
		(sleep (* 0.08 (max 0 coeff))))
    (funcall (buzzer-actuator d) nil)))

(defun buzz (action)
  (with-open-file (s "/sys/class/leds/green_l/brightness" :direction :output :if-exists :append)
    (write-string (if action "255" "0") s)))

(defmethod initialize-display ((d display))
  (setf (buzzer-actuator d) 'buzz)
  (write-lcd d (format nil "~a~a~a" +lcd-init+ +lcd-cursor-off+ +lcd-blink-off+)))

(defun dummy-menu-item (d item)
  (declare (ignore d item))
  (print "Called a dummy"))

(defmethod menu-interaction ((d display) entry-menu)
  (initialize-display d)
  (let ((position 0)
	(menu (cdr entry-menu))
	menu-stack
	rise-event)
    (writeout-screen d (car (aref menu position)))
    (cl-evdev:with-evdev-devices (event "/dev/input/event2" "/dev/input/by-path/platform-keys-event")
      (when (eql (class-name (class-of event)) 'cl-evdev:keyboard-event)
	(cond ((and (not rise-event) (eql (state event) :pressed))
	       (setf rise-event event))
	      ((and rise-event
		    (eql (state event) :released)
		    (eql (name event) (name rise-event)))
	       (setf rise-event nil)
	       (case (cl-evdev:name event)
		 (0
		  (case (cdr (aref menu position))
		    (:descend
		     (push (cons position menu) menu-stack)
		     (setf menu (cdar (aref menu position))
			   position 0))
		    (:exit
		     (destructuring-bind (p . m) (pop menu-stack)
		       (setf position p
			     menu m)))
		    (otherwise
		     (funcall (cdr (aref menu position)) d (car (aref menu position)))))
		  (write-lcd d +lcd-cls+)
		  (writeout-screen d (if (eql :descend (cdr (aref menu position)))
					 (caar (aref menu position))
					 (car (aref menu position)))))
		 ((cl-evdev::f1 cl-evdev::f2)
		  (let ((new-position (alexandria:clamp
				       (+ position (if (eql (name event) 'cl-evdev::f1) -1 1))
				       0 (1- (length menu)))))
		    (unless (= new-position position)
		      (setf position new-position)
		      (let* ((item (car (aref menu position)))
			     (text (cond ((null (car item)) *up-menu-message*)
					 ((arrayp (cdr item)) (car item))
					 (t item))))
			(funcall (if (eql (name event) 'cl-evdev::f1) 'swipe-right 'swipe-left)
				 d text))))))))))))

(defmethod make-menu-text ((d display) &rest lines)
  (loop repeat (height d)
	 for line in lines
	 collecting (setf (subseq (make-array (width d)
					      :initial-element #\Space
					      :element-type 'character)
				  0 (length line))
			  line))))
