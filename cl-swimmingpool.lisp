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
   (%runningp
    :accessor runningp
    :initform nil)
   (%evaluation-values
    :accessor evaluation-values
    :initform nil)
   (%function-to-execute
    :accessor function-to-execute
    :initform nil))
  (:metaclass metalock:metalock));;no need to worry about locks now

(defun make-swimmer ()
  (let ((t-o (make-instance 'swimmer)))
    (setf (thread t-o) (bt:make-thread (lambda () (wait-for-function t-o))))
    t-o))

(defclass armband ()
  ((%swimmer
    :accessor swimmer
    :initarg :swimmer)
   (%unique-id
    :accessor id
    :initarg :id)
   (%entry-time
    :accessor entry-time
    :initform (get-universal-time))
   (%exit-time
    :accessor exit-time)))

(defun make-armband (swimmer id)
  (make-instance 'armband :swimmer swimmer :id id))

;;;we want a threadpool where you can (dive <lambda>) and the lambda is placed into a
;;;waiting swimmer where the function is passed to the thread, executed and the
;;;result stored within the object

(defmacro execute-when ((when) &body body)
  "Executes body when WHEN returns non nil. blocks until it does."
  `(do ()(,when ,@body)
     (sleep 0.0001)))

(defmacro execute-when-and-keep-going ((when) &body body)
  `(loop :do (execute-when (,when) ,@body)))

(defmethod execute-function ((swimmer swimmer))
  (setf (runningp swimmer) t)
  ;;will need something here to make sure that the thread hasn't fuckin died
  (setf (evaluation-value swimmer)
        (funcall (function-to-execute swimmer)))
  (setf (function-to-execute swimmer) nil)
  (setf (runningp swimmer) nil))

(defmethod pass-function ((swimmer swimmer) (function function))
  (execute-when ((not (runningp swimmer)))
    (setf (function-to-execute swimmer) function)
    (let ((sym (gensym)))
      (setf (evaluation-values swimmer)
            (append (evaluation-values swimmer) (list sym))))))

(defmethod get-return-value ((swimmer swimmer) id)
  "Grabs the evaluation-value from SWIMMER if there is one."
  (let* ((values (evaluation-values swimmer))
         (value (second (assoc id values :test #'eq))))
    (if value
        (setf (evaluation-values swimmer) (remove value values :test #'tree-equal))
        ;;reset the val to nil
        (error 'no-value-for-key :key id))))

;;;might be a good idea to have a flag to say that the value has been collected.

(defmethod wait-for-function ((swimmer swimmer))
  "Sits and waits until there is a function for the SWIMMER to evaluate, when one 
is set 'execute-function' is called and this goes back to waiting. This is designed to be
used by the thread."
  (execute-when-and-keep-going ((function-to-execute swimmer))
    (execute-function swimmer)))

(defmethod drown ((swimmer swimmer))
  "Currently just stops the thread within SWIMMER and sets the thread object to 
no longer running."
  (bt:destroy-thread (thread swimmer))
  (setf (runningp swimmer) nil))

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
  (let ((swimmer)
        (done nil));;this is less than efficient but pools are quite small so its
    ;;trivial to map over them
    (map-threads (swimming-pool)
                 (lambda (key swimmer)
                   (unless (or (runningp swimmer) done)
                     (pass-function swimmer function)
                     (setf done t)
                     (setf swimmer (make-armband swimmer)))))
    (if done
        swimmer
        nil)))

(defmethod get-out ((armband armband))
  (get-return-value (swimmer armband) (id armband)))
