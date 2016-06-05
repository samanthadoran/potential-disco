(in-package :cl-user)

(defpackage #:NES-controller
  (:nicknames #:controller)
  (:use :cl :cl-user)
  (:export #:make-controller #:read-controller #:write-controller
           #:update-controller #:controller #:controller-buttons-callback))

(in-package :NES-controller)
(declaim (optimize (speed 3) (safety 1)))

(defstruct controller
  (buttons
   (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) 1))
  (index 0 :type (unsigned-byte 3))
  (strobe 0 :type (unsigned-byte 1))
  (buttons-callback (lambda()) :type function))

(defun read-controller (c)
  (declare (controller c))
  (let ((value (aref (controller-buttons c) (controller-index c))))
    (setf
     (controller-index c)
     (if (not (ldb-test (byte 1 0) (controller-strobe c)))
       (ldb (byte 3 0) (1+ (controller-index c)))
       0))
    value))

(defun write-controller (c val)
  (declare (controller c) ((unsigned-byte 8) val))
  (when (ldb-test (byte 1 0) (setf (controller-strobe c) (ldb (byte 1 0) val)))
    (setf (controller-index c) 0)))

(defun update-controller (c)
  (declare (controller c))
  (when (ldb-test (byte 1 0) (controller-strobe c))
    (setf (controller-buttons c) (the (simple-array (unsigned-byte 8) 1)(funcall (controller-buttons-callback c))))))
