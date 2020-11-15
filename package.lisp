;;;; package.lisp

(defpackage #:cl-swimmingpool
  (:use #:cl)
  (:nicknames #:swim)
  (:export #:dive
           #:get-out
           #:make-swimming-pool
           #:in-progress
           #:function-execution-errored))
