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

(defclass display ()
  ((path :accessor path :initarg :path
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

(defmethod swipe-right ((d display) new-lines)
  (write-lcd d +lcd-cursor-home+)
  (loop for pos from 0 to 11
	for coeff downfrom 1 by 0.2
	do (write-lcd d +lcd-scroll-right+)
	   (when (and new-lines (> pos 5))
	     (write-lcd d (car new-lines))
	     (setf new-lines (cdr new-lines))
	     (write-lcd d (format nil "~a~%" +lcd-kill+)))
	   (sleep (* 0.1 (max 0 coeff)))))

(defparameter *m1* '("A very important txt" "  On a few lines" "  This is four lines" "  To be precise"))

(defparameter *m2* '("Another striking txt" "  Also short" "  Still four lines" "  Working flawlessly"))



(defun test ()
  ;(write-lcd *lcd* +lcd-cls+)
  (loop repeat 4 do (write-lcd *lcd* (format nil "Hullo Guvnor nice!~%"))))
