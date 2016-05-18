(in-package :6502-cpu)
(declaim (optimize (speed 3) (safety 1)))
(defun sei (c inst)
  (declare (cpu c) (instruction inst))
  "SEI: set interrupt flag"
  (declare (ignore inst))
  (setf (flags-interrupt (cpu-sr c)) T))

(defun sec (c inst)
  (declare (cpu c) (instruction inst))
  "SEC: set carry flag"
  (declare (ignore inst))
  (setf (flags-carry (cpu-sr c)) T))

(defun clv (c inst)
  (declare (cpu c) (instruction inst))
  "CLV: clear overflow flag"
  (declare (ignore inst))
  (setf (flags-overflow (cpu-sr c)) nil))

(defun clc (c inst)
  (declare (cpu c) (instruction inst))
  "CLC: clear carry flag"
  (declare (ignore inst))
  (setf (flags-carry (cpu-sr c)) nil))

(defun cld (c inst)
  (declare (cpu c) (instruction inst))
  "Clear decimal flag"
  (declare (ignore inst))
  (setf (flags-bcd (cpu-sr c)) nil))

(defun sed (c inst)
  (declare (cpu c) (instruction inst))
  "Set decimal flag"
  (declare (ignore inst))
  (setf (flags-bcd (cpu-sr c)) T))
