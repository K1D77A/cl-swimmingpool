;;;; cl-swimmingpool.asd

(asdf:defsystem #:cl-swimmingpool
  :description "A trivial thread pooling library that allows you to queue work to be done"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:metalock
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "cl-swimmingpool")))
