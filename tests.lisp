(in-package #:menuett)

(defparameter *m1* '("A very important txt" "  On a few lines" "  This is four lines" "  To be precise"))

(defparameter *m2* '("Another striking txt" "  Also short" "  Still four lines" "  Working flawlessly"))

(defun test ()
  ;(write-lcd *lcd* +lcd-cls+)
  (loop repeat 4 do (write-lcd *lcd* (format nil "Hullo Guvnor nice!~%"))))


(defun test2 ()
  (initialize *lcd*)
  (loop
    (swipe-right *lcd* *m1*)
    (sleep 1)
    (swipe-left *lcd* *m2*)
    (sleep 1)))
