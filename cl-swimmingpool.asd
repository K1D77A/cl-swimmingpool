;;;; cl-swimmingpool.asd

(asdf:defsystem #:cl-swimmingpool
  :description "A trivial thread pooling library that allows you to queue work to be done"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.2"
  :depends-on (#:metalock
               #:alexandria
               #:trivial-backtrace)
  :serial t
  :components ((:file "package")
               (:file "classes&conditions")
               (:file "cl-swimmingpool")))
