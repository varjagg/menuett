;;;; menuett.asd

(asdf:defsystem #:menuett
  :description "Describe menuett here"
  :author "Eugene Zaikonnikov"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "menuett")))
