(in-package :cl-user)

(defpackage #:6502-cpu
  (:nicknames #:cpu)
  (:use :cl :cl-user)
  (:export #:make-cpu #:pages-differ #:reset #:power-on #:pull-stack
           #:push-stack #:pull16 #:push16 #:cpu-cycles #:cpu-accumulator #:cpu-x
           #:cpu-y #:cpu-pc #:cpu-sp #:cpu-memory))

(in-package :6502-cpu)

;(defstruct flags
;  "Flag register")

(defstruct cpu
  "A model 6502"
  (cycles 0 :type (unsigned-byte 16))
  (accumulator 0 :type (unsigned-byte 8))
  (x 0 :type (unsigned-byte 8))
  (y 0 :type (unsigned-byte 8))
  (pc 0 :type (unsigned-byte 16))
  (sp 0 :type (unsigned-byte 8))
  (memory (make-array #x800 :element-type '(unsigned-byte 8))))

(defun wrap-byte (val)
  (logand #xFF val))

(defun wrap-word (val)
  (logand #xFFFF val))

(defun pages-differ (a b)
  (declare ((unsigned-byte 16) a b))
  (not
   (=
    (logand a #xFF00)
    (logand b #xFF00))))

(defun reset (c)
  "Reset state of cpu"
  (setf (cpu-sp c) (wrap-byte (- (cpu-sp c) 3))))

(defun power-on (c)
  "Power on state of cpu"
  ;Need to set status
  (setf (cpu-sp c) #xFD))

(defun pull-stack (c)
  "Empty stack pull"
  (setf (cpu-sp c) (wrap-byte (1+ (cpu-sp c))))
  (aref (cpu-memory c) (logior (cpu-sp c) #x100)))

(defun push-stack (c val)
  "Put a value on the stack and then push it forwards"
  (declare ((unsigned-byte 8) val))
  (setf
   (aref
    (cpu-memory c)
    (logior (cpu-sp c) #x100))
   val)
  (setf
   (cpu-sp c)
   (wrap-byte (1- (cpu-sp c)))))

(defun pull16 (c)
  "Pull twice and make a 16 bit address."
  (logior (pull-stack c) (ash (pull-stack c) 8)))

(defun push16 (c val)
  "Push twice."
  (push-stack c (wrap-byte (ash val -8)))
  (push-stack c (wrap-byte val)))
