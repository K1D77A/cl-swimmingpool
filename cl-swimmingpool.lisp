;;;; cl-swimmingpool.lisp

(in-package #:cl-swimmingpool)

(defparameter *sleep-time* 0.001)

(defmethod ready-to-execute-p ((floatation plastic-float))
  (and (not (runningp floatation))
       (not (slot-boundp floatation '%result))
       (slot-boundp floatation '%function-to-execute)))

(defmethod execute-function ((floatation plastic-float))
  (with-accessors ((start-time start-time)
                   (end-time end-time)
                   (runningp runningp)
                   (result result)
                   (function-to-execute function-to-execute)
                   (backtrace backtrace)
                   (erroredp erroredp))
      floatation
    (setf runningp t)
    (setf start-time (get-universal-time))
    (let ((res
            (handler-case (funcall function-to-execute)
              (condition (c)
                (setf backtrace (trivial-backtrace:print-backtrace c :output nil))
                (setf erroredp t)
                c))))
      (setf result res)
      (setf end-time (get-universal-time))
      (setf runningp nil))))

(defmethod execute-function ((swimmer swimmer))
  (let ((to-execute (remove-if-not #'ready-to-execute-p (floatation-devices swimmer))))
    (if to-execute;if there are none to execute then sleep, this is cos they have to be
        (mapc #'execute-function to-execute);removed
        (sleep *sleep-time*))))

(defmethod pass-function ((swimmer swimmer) (plastic-float plastic-float))
  (when (shutdownp swimmer)
    (error 'swimmer-has-or-is-trying-to-shutdown))
  (setf (floatation-devices swimmer)
        (append (list plastic-float) (floatation-devices swimmer))))

(defmethod wait-for-function ((swimmer swimmer))
  "Loops constantly waiting for plastic-floats to be added to SWIMMER. Once a plastic-float
is added this function starts the execution of that plastic-float and any others that may
have been added. This function will also remove any plastic-floats that have had their 
results read."
  (do* ((floats (floatation-devices swimmer)
                (floatation-devices swimmer))
        (ready (some #'ready-to-execute-p floats)
               (some #'ready-to-execute-p floats)))
       
       ((shutdownp swimmer)
        t)
    (if ready
        (execute-function swimmer)
        (let ((readp (some #'result-read-p floats)))
          (when readp
            (setf (floatation-devices swimmer);;trim all those already read
                  (remove-if #'result-read-p (floatation-devices swimmer))))
          (sleep *sleep-time*)))))

(defmethod get-out ((plastic-float plastic-float))
  "Retrieves the result from PLASTIC-FLOAT. If PLASTIC-FLOAT is still being executed then
the condition 'in-progress' is signalled. If PLASTIC-FLOAT errored while executing then 
'function-execution-errored' is signalled, in that circumstance you can use the accessors
to inspect the contents of the PLASTIC-FLOAT; (result ..) will be the condition, 
(backtrace ..) a string backtrace."
  (with-accessors ((result result)
                   (result-read-p result-read-p))
      plastic-float
    (if (and (slot-boundp plastic-float '%result)
             (not (erroredp plastic-float))
             (not (result-read-p plastic-float)))
        (progn (setf result-read-p t)
               result)
        (if (erroredp plastic-float)
            (progn 
              (setf result-read-p t)
              (error 'function-execution-errored))
            (error 'in-progress)))))

(defmethod drown ((swimmer swimmer) &optional (dirty nil))
  "Sets shutdownp in SWIMMER to t, meaning once it has finished executing whatever task
it last received it will shut itself down. If the flag DIRTY is set to non nil this will
use bt:destroy-thread."
  (when dirty
    (bt:destroy-thread (thread swimmer)))
  (setf (shutdownp swimmer) t))


(defmethod bleach ((swimming-pool swimming-pool) &optional (dirty nil))
  "Shuts down the threads in swimming-pool by calling 'drown' on each swimmer. If you set
DIRTY to non nil then the bt:destroy-thread will be called on each thread. "
  (mapc (lambda (swimmer)
          (drown swimmer dirty))
        (swimmers swimming-pool)))

(defmethod dive ((swimming-pool swimming-pool) (function function))
  "Creates a plastic-float using FUNCTION and hands it over to the swimmer with the
fewest plastic-floats."
  (let* ((swimmer (alexandria:extremum (swimmers swimming-pool)
                                       #'<
                                       :key (lambda (x)
                                              (length (floatation-devices x)))))
         (float (make-plastic-float swimmer (gensym) function)))
    (pass-function swimmer float)
    float))



