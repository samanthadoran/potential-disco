(in-package :cl-user)

(defpackage #:NES-controller
  (:nicknames #:controller)
  (:use :cl :cl-user)
  (:export #:make-controller #:read-controller #:write-controller
           #:update-controller #:controller))

(in-package :NES-controller)
(declaim (optimize (speed 3) (safety 1)))

(defstruct controller
  (buttons
   (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) 1))
  (index 0 :type (unsigned-byte 3))
  (strobe 0 :type (unsigned-byte 1)))

(defun read-controller (c)
  (declare (controller c))
  (let ((value (aref (controller-buttons c) (controller-index c))))
    (setf
     (controller-index c)
     (if (not (ldb-test (byte 1 0) (controller-strobe c)))
       (logand 7 (1+ (controller-index c)))
       0))
    value))

(defun write-controller (c val)
  (declare (controller c) ((unsigned-byte 8) val))
  (when (ldb-test (byte 1 0) (setf (controller-strobe c) (ldb (byte 1 0) val)))
    (setf (controller-index c) 0)))

(defun update-controller (c buttons)
  (declare (controller c) ((simple-array (unsigned-byte 8) 1)))
  (when (ldb-test (byte 1 0) (controller-strobe c))
    (setf (controller-buttons c) buttons)))
