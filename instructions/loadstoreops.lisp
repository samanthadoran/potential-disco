(in-package :6502-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defun ldy (c inst)
  (declare (cpu c) (instruction inst))
  "LDY. Load value to y"
  (set-zn c (setf (cpu-y c) (get-value c inst))))

(defun lda (c inst)
  (declare (cpu c) (instruction inst))
  "LDA. Load value to accumulator"
  (set-zn c (setf (cpu-accumulator c) (get-value c inst))))

(defun ldx (c inst)
  (declare (cpu c) (instruction inst))
  "LDX. Load value to cpu-x"
  (set-zn c (setf (cpu-x c) (get-value c inst))))

(defun sty (c inst)
  (declare (cpu c) (instruction inst))
  (write-cpu c (get-address c inst) (cpu-y c)))

(defun sta (c inst)
  (declare (cpu c) (instruction inst))
  (write-cpu c (get-address c inst) (cpu-accumulator c)))

(defun stx (c inst)
  (declare (cpu c) (instruction inst))
  (write-cpu c (get-address c inst) (cpu-x c)))

(defun tax (c inst)
  (declare (cpu c) (instruction inst))
  "TAX. Transfer accumulator to x"
  (declare (ignore inst))
  (set-zn c (setf (cpu-x c) (cpu-accumulator c))))

(defun tay (c inst)
  (declare (cpu c) (instruction inst))
  "TAY. Transfer accumulator to y"
  (declare (ignore inst))
  (set-zn c (setf(cpu-y c) (cpu-accumulator c))))

(defun txa (c inst)
  (declare (cpu c) (instruction inst))
  "TXA. Transfer x to accumulator"
  (declare (ignore inst))
  (set-zn c (setf (cpu-accumulator c) (cpu-x c))))

(defun tya (c inst)
  (declare (cpu c) (instruction inst))
  "TYA. Transfer y to accumulator"
  (declare (ignore inst))
  (set-zn c (setf (cpu-accumulator c) (cpu-y c))))

(defun tsx (c inst)
  (declare (cpu c) (instruction inst))
  "TSX. Transfer stack to x"
  (declare (ignore inst))
  (set-zn c (setf (cpu-x c) (cpu-sp c))))

(defun txs (c inst)
  (declare (cpu c) (instruction inst))
  "TXS. Transfer x to stack"
  (declare (ignore inst))
  (set-zn c (setf (cpu-sp c) (cpu-x c))))

(defun php (c inst)
  (declare (cpu c))
  (declare (ignore inst))
  (push-stack c (logior #x10 (the (unsigned-byte 8)(make-byte-from-flags (cpu-sr c))))))

(defun pha (c inst)
  (declare (cpu c) (instruction inst))
  (declare (ignore inst))
  (push-stack c (cpu-accumulator c)))

(defun pla (c inst)
  (declare (cpu c) (instruction inst))
  (declare (ignore inst))
  (set-zn c (setf (cpu-accumulator c) (pull-stack c))))

(defun plp (c inst)
  (declare (cpu c) (instruction inst))
  (declare (ignore inst))
  (setf (cpu-sr c) (make-flags-from-byte (logior #x20 (logand #xEF (pull-stack c))))))
