(in-package :6502-cpu)

(defun sei (c inst)
  "SEI: set interrupt flag"
  (declare (ignore inst))
  (setf
   (flags-interrupt (cpu-sr c))
   T)
  (format
   nil
   "SEI. Set the interrupt flag"))

(defun cld (c inst)
  "Clear decimal flag"
  (declare (ignore inst))
  (setf
   (flags-bcd (cpu-sr c))
   nil)
  (format
   nil
   "CLD. Cleared the decimal flag"))
