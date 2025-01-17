(in-package #:menuett)

(defparameter *m1* '("A very important txt" "  On a few lines" "  This is four lines" "  To be precise"))

(defparameter *m2* '("Another striking txt" "  Also short" "  Still four lines" "  Working flawlessly"))

(defparameter *frame* (list (format nil "~c------------------~c" (code-char 162) (code-char 96))
			    "|                  |"
			    "|                  |"
			    (format nil"~c__________________~c" (code-char 164) (code-char 163))))

(alexandria:define-constant +angle-top-right+ (escaped "LG01c04040404040404;")
  :test #'string-equal)

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

(defparameter *sub1* '("Submenu Item 1" "Parameter 1" "Parameter 2" "Summary"))
(defparameter *sub2* '("Submenu Item 2" "Parameter 1" "Parameter 2" "Summary"))

(defparameter *tm0* (cons (list "SUBMENU 1" "" "Click to descend" "") (make-array 3 :initial-contents `((,*sub1* . dummy-menu-item) (,*sub2* . dummy-menu-item) (nil . :exit)))))

(defparameter *tm1* (cons (list "Main Menu" "" "" "") (make-array 3 :initial-contents `((,*m1* . dummy-menu-item) (,*m2* . dummy-menu-item) (,*tm0* . :descend)))))
