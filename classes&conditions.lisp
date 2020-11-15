
(in-package #:cl-swimmingpool)

(define-condition in-progress ()
  ())
(define-condition function-execution-errored ()
  ())
(define-condition swimmer-has-or-is-trying-to-shutdown ()
  ())

(defun ascii-swimmer (stream)
  (format stream
          "     
     .-'~~'-.
   .'        '.
  /  .------.  \
  | / _    _ \ |
  |==(_)==(_)==|
  \\/     \    \\/
   :    ^^    ;
    \\  '=='  /
     `-.__.-'" ))

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

(defmethod print-object ((pool swimming-pool) stream)
  (print-unreadable-object (pool stream)
    (format stream "Swimming pool has ~A swimmers.~A"
            (length (swimmers pool))
            (ascii-swimmer nil))))

(defclass swimmer ()
  ((%thread
    :accessor thread)
   (%floatation-devices
    :accessor floatation-devices
    :initform nil)
   (%shutdownp
    :accessor shutdownp
    :initform nil))
  (:metaclass metalock:metalock));;no need to worry about locks now

(defun make-swimmer ()
  (let ((t-o (make-instance 'swimmer)))
    (setf (thread t-o) (bt:make-thread (lambda () (wait-for-function t-o))))
    t-o))

(defmethod print-object ((swimmer swimmer) stream)
  (print-unreadable-object (swimmer stream :type t)
    (format stream "Swimmer is currently ~A and has ~A plastic-floats.~%"
            (if (shutdownp swimmer)
                "out/getting out"
                "swimming")
            (length (floatation-devices swimmer)))))

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

(defmethod print-object ((float plastic-float) stream)
  (print-unreadable-object (float stream :type t)
    (format stream "Plastic float is currently ~A and it ~A errored."
            (if (runningp float)
                "with a swimmer"
                "out of the pool")
            (if (erroredp float)
                "has"
                "has not"))))
