;;;; package.lisp

(defpackage #:cl-swimmingpool
  (:use #:cl)
  (:nicknames #:swim)
  (:export #:dive
           #:get-out
           #:make-swimming-pool
           #:in-progress
           #:function-execution-errored
           #:bleach
           #:drown
           #:swimmer-has-or-is-trying-to-shutdown

           ;;plastic-float accessors
           #:backtrace
           #:end-time
           #:erroredp
           #:function-to-execute
           #:result
           #:result-read-p
           #:runningp
           #:start-time
           #:swimmer
           #:unique-id))

