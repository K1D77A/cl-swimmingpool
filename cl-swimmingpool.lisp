;;;; cl-swimmingpool.lisp

(in-package #:cl-swimmingpool)

(defparameter *sleep-time* 0.001)

(define-condition in-progress ()
  ())
(define-condition function-execution-errored ()
  ())

(defclass swimming-pool ()
  ((%swimmers-count
    :accessor swimmers-count
    :initarg :swimmers-count
    :initform 1)
   (%swimmers
    :accessor swimmers
    :initarg :swimmers))
  (:metaclass metalock:metalock))

(defun make-swimming-pool (&optional (pool-size 1))
  (let* ((pool (make-instance 'swimming-pool :swimmers-count pool-size)))
    (setf (swimmers pool)
          (mapcar (lambda (x)
                    (declare (ignore x))
                    (make-swimmer))
                  (make-list pool-size)))
    pool))

(defclass swimmer ()
  ((%thread
    :accessor thread)
   (%floation-devices
    :accessor floatation-devices
    :initform nil))
  (:metaclass metalock:metalock));;no need to worry about locks now

(defun make-swimmer ()
  (let ((t-o (make-instance 'swimmer)))
    (setf (thread t-o) (bt:make-thread (lambda () (wait-for-function t-o))))
    t-o))

(defclass plastic-float ()
  ((%swimmer
    :accessor swimmer
    :initarg :swimmer)
   (%unique-id
    :accessor id
    :initarg :id)
   (%runningp
    :accessor runningp
    :initform nil)
   (%erroredp
    :accessor erroredp
    :initform nil)
   (%backtrace
    :accessor backtrace)
   (%result-read-p
    :accessor result-read-p
    :initform nil)
   (%function-to-execute
    :accessor function-to-execute
    :initarg :function-to-execute)
   (%result
    :accessor result)
   (%start-time
    :accessor start-time)
   (%end-time
    :accessor end-time)))

(defun make-plastic-float (swimmer id function)
  (make-instance 'plastic-float :swimmer swimmer
                                :id id
                                :function-to-execute function))

;;;we want a threadpool where you can (dive <lambda>) and the lambda is placed into a
;;;waiting swimmer where the function is passed to the thread, executed and the
;;;result stored within the object

(defmacro execute-when ((when) &body body)
  "Executes body when WHEN returns non nil. blocks until it does."
  `(do ()(,when ,@body)
     (sleep 0.0001)))

(defmacro execute-when-and-keep-going ((when) &body body)
  `(loop :do (execute-when (,when) ,@body)))

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
    ;;still need something here to make sure the thread doesnt get yeeted
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
       
       (())
    (if ready
        (execute-function swimmer)
        (let ((readp (some #'result-read-p floats)))
          (when readp
            (setf (floatation-devices swimmer);;trim all those already read
                  (remove-if #'result-read-p (floatation-devices swimmer))))
          (sleep *sleep-time*)))))

(defmethod get-out ((plastic-float plastic-float))
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

(defmethod drown ((swimmer swimmer))
  "Currently just stops the thread within SWIMMER and sets the thread object to 
no longer running."
  (bt:destroy-thread (thread swimmer)))


;;;okay this is working as planned although there is no recourse for when a thread dies
;;;but for later, its also possible to add in the ability to block new evaluations until
;;;values have been grabbed by 'get-out'

(defmacro map-threads ((pool) function)
  `(maphash ,function (swimmers ,pool)))

(defmethod bleach ((swimming-pool swimming-pool))
  "Shuts down the threads in swimming-pool"
  (map-threads (swimming-pool) 
               (lambda (key val)
                 (declare (ignore key))
                 (drown val))))

;;;do I want this to block or not? What if there are no threads ready to go? Do we keep
;;;checking or do we just error? idk
(defmethod dive ((swimming-pool swimming-pool) (function function))
  ""    
  (let* ((swimmer (alexandria:extremum (swimmers swimming-pool)
                                       #'<
                                       :key (lambda (x)
                                              (length (floatation-devices x)))))
         (float (make-plastic-float swimmer (gensym) function)))
    (pass-function swimmer float)
    float))



