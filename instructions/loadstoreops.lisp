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
