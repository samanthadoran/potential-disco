(in-package :cl-user)

(defpackage #:6502-cpu
  (:nicknames #:cpu)
  (:use :cl :cl-user)
  (:export #:make-cpu #:pages-differ #:reset #:power-on #:pull #:cpu-cycles
           #:cpu-accumulator #:cpu-x #:cpu-y #:cpu-pc #:cpu-sp #:cpu-memory))

(in-package :6502-cpu)

(defstruct cpu
  "A model 6502"
  (cycles 0 :type (unsigned-byte 16))
  (accumulator 0 :type (unsigned-byte 8))
  (x 0 :type (unsigned-byte 8))
  (y 0 :type (unsigned-byte 8))
  (pc 0 :type (unsigned-byte 16))
  (sp 0 :type (unsigned-byte 8))
  (memory (make-array #x800 :element-type '(unsigned-byte 8))))

(defun pages-differ (a b)
  (declare ((unsigned-byte 16) a b))
  (not
   (=
    (logand a #xFF00)
    (logand b #xFF00))))

(defun reset (c)
  "Reset state of cpu"
  (setf (cpu-sp c) (- (cpu-sp c) 3)))

(defun power-on (c)
  "Power on state of cpu"
  ;Need to set status
  (setf (cpu-sp c) #xFD))

(defun pull (c)
  "Empty stack pull"
  (setf (cpu-sp c) (+ (cpu-sp c) 1))
  (aref (cpu-memory c) (logior (cpu-sp c) #x100)))
