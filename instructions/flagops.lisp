(in-package :6502-cpu)
(declaim (optimize (speed 3) (safety 1)))
(defun sei (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "SEI: set interrupt flag"
  (setf (flags-interrupt (cpu-sr c)) T))

(defun sec (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "SEC: set carry flag"
  (setf (flags-carry (cpu-sr c)) T))

(defun clv (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "CLV: clear overflow flag"
  (setf (flags-overflow (cpu-sr c)) nil))

(defun clc (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "CLC: clear carry flag"
  (setf (flags-carry (cpu-sr c)) nil))

(defun cld (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "Clear decimal flag"
  (setf (flags-bcd (cpu-sr c)) nil))

(defun sed (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "Set decimal flag"
  (setf (flags-bcd (cpu-sr c)) T))
