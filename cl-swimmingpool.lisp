;;;; cl-swimmingpool.lisp

(in-package #:cl-swimmingpool)

(define-condition in-progress ()
  ())

(define-condition no-value-for-key ()
  ((%key
    :accessor key
    :initarg :key)))

(defclass swimming-pool ()
  ((%thread-count
    :accessor thread-count
    :initarg :thread-count
    :initform 1)
   (%thread-pool
    :accessor thread-pool
    :initform (make-hash-table :test #'eq)))
  (:metaclass metalock:metalock))

(defun make-swimming-pool (&optional (pool-size 1))
  (let* ((pool (make-instance 'swimming-pool :thread-count pool-size))
         (thread-pool (thread-pool pool)))
    (dotimes (i pool-size)
      (setf (gethash (gensym) thread-pool) (make-swimmer)))
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
   (%result-read-p
    :accessor result-read-p
    :initform nil)
   (%function-to-execute
    :accessor function-to-execute)
   (%result
    :accessor result)
   (%start-time
    :accessor start-time)
   (%end-time
    :accessor end-time)))

(defun make-plastic-float (swimmer id)
  (make-instance 'plastic-float :swimmer swimmer :id id))

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
       (slot-unbound 'plastic-float floatation '%result)))



(defmethod execute-function ((floatation plastic-float))
  (with-accessors ((start-time start-time)
                   (end-time end-time)
                   (runningp runningp)
                   (result result)
                   (function-to-execute function-to-execute))
      floatation
    (setf runningp t)
    (setf start-time (get-universal-time))
    ;;still need something here to make sure the thread doesnt get yeeted
    (setf result (funcall function-to-execute))
    (setf end-time (get-universal-time))
    (setf runningp nil)))

(defmethod execute-function ((swimmer swimmer))
  (execute-when ((not (zerop (length (floatation-devices swimmer)))))
    (loop :for floatation :in (floatation-devices swimmer)
          :if (ready-to-execute-p floatation)
            :do (execute-function floatation)
                (return floatation)
          :else :do (return nil))))

(defmethod pass-function ((swimmer swimmer) (plastic-float plastic-float))
  (setf (floatation-devices swimmer)
        (append plastic-float (floatation-devices swimmer))))

(defmethod wait-for-function ((swimmer swimmer))
  "Sits and waits until there is a function for the SWIMMER to evaluate, when one 
is set 'execute-function' is called and this goes back to waiting. This is designed to be
used by the thread."
  (execute-when-and-keep-going ((floatation-devices swimmer))
    (let ((executed (execute-function swimmer)))
      (when (and executed (result-read-p executed))
        (setf (floatation-devices swimmer)
              (remove executed (floatation-devices swimmer)))))))

(defmethod get-out ((plastic-float plastic-float))
  (with-accessors ((result result)
                   (result-read-p result-read-p))
      plastic-float
    (setf result-read-p t)
    result))

(defmethod drown ((swimmer swimmer))
  "Currently just stops the thread within SWIMMER and sets the thread object to 
no longer running."
  (bt:destroy-thread (thread swimmer)))


;;;okay this is working as planned although there is no recourse for when a thread dies
;;;but for later, its also possible to add in the ability to block new evaluations until
;;;values have been grabbed by 'get-out'

(defmacro map-threads ((pool) function)
  `(maphash ,function (thread-pool ,pool)))

(defmethod bleach ((swimming-pool swimming-pool))
  "Shuts down the threads in swimming-pool"
  (map-threads (swimming-pool) 
               (lambda (key val)
                 (declare (ignore key))
                 (drown val))))

;;;do I want this to block or not? What if there are no threads ready to go? Do we keep
;;;checking or do we just error? idk
(defmethod dive ((swimming-pool swimming-pool) (function function))
"Adds a function to a swimmer that is ready to receive it, returns a symbol which
is used to get the final value of the function once it is evaluated in a thread."
(let ((plastic-float)
      (done nil));;this is less than efficient but pools are quite small so its
  ;;trivial to map over them
  (map-threads (swimming-pool)
               (lambda (key swimmer)
                 (unless (or (runningp swimmer) done)
                   (let ((sym (pass-function swimmer function)))
                     (setf done t)
                     (setf plastic-float (make-plastic-float swimmer sym))))))
  (if done
      plastic-float
      nil)))



