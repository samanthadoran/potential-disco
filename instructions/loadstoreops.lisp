(in-package :6502-cpu)

(defun ldy (c inst)
  "LDY. Load value to y"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (setf
     (cpu-y c)
     val)
    (set-zn c (cpu-y c))
    (format
     nil
     "LDY with mode ~a~@[ from 0x~x~] loaded value 0x~x into cpu-y"
     mode (when (not (equal mode :immediate)) addr) val)))

(defun lda (c inst)
 "LDA. Load value to accumulator"
 (let ((mode (instruction-addressing-mode inst))
       (val (get-value c inst))
       (addr (get-address c inst)))
   (setf
    (cpu-accumulator c)
    val)
   (set-zn c (cpu-accumulator c))
   (format
    nil
    "LDA with mode ~a~@[ from 0x~x~] loaded value 0x~x into accumulator"
    mode (when (not (equal mode :immediate)) addr) val)))

(defun ldx (c inst)
 "LDX. Load value to cpu-x"
 (let ((mode (instruction-addressing-mode inst))
       (val (get-value c inst))
       (addr (get-address c inst)))
   (setf
    (cpu-x c)
    val)
   (set-zn c (cpu-x c))
   (format
    nil
    "LDX with mode ~a~@[ from 0x~x~] loaded value 0x~x into cpu-x"
    mode (when (not (equal mode :immediate)) addr) val)))

(defun sty (c inst)
  (let ((addr (get-address c inst)))
    (write-cpu c addr (cpu-y c))
    (format
     nil
     "STY stored 0x~x at 0x~x" (cpu-y c) addr)))

(defun sta (c inst)
 (let ((addr (get-address c inst)))
   (write-cpu c addr (cpu-accumulator c))
   (format
    nil
    "STA stored 0x~x at 0x~x" (cpu-accumulator c) addr)))

(defun stx (c inst)
  (let ((addr (get-address c inst)))
    (write-cpu c addr (cpu-x c))
    (format
     nil
     "STX stored 0x~x at 0x~x" (cpu-x c) addr)))

(defun txa (c inst)
  "TXA. Transfer x to accumulator"
  (declare (ignore inst))
  (setf
   (cpu-accumulator c)
   (cpu-x c))
  (format
   nil
   "TXA changed accumulator to 0x~x" (cpu-accumulator c)))

(defun txs (c inst)
  "TXS. Transfer x to stack"
  (declare (ignore inst))
  (setf
   (cpu-sp c)
   (cpu-x c))
  (format
   nil
   "TXS changed cpu-sp to 0x~x" (cpu-sp c)))

(defun php (c)
  (push-stack c (make-byte-from-flags (cpu-sr c)))
  (format
   nil
   "PHP pushed processor flags to the stack"))
(defun plp (c)
  (setf (cpu-sr c) (make-flags-from-byte (logand #xEF (pull-stack c))))
  (format
   nil
   "PLP pulled processor flags from the stack"))
