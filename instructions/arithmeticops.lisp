(in-package :6502-cpu)

(defun ora (c inst)
  "ORA: or value with accumulator"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (setf
     (cpu-accumulator c)
     (logior (cpu-accumulator c) val))
    (set-zn c (cpu-accumulator c))
  (format
   nil
   "ORA with mode ~a from address ~x that holds value ~x. Produces value ~x"
   mode addr val (cpu-accumulator c))))

(defun anda (c inst)
  "ANDA: and value with accumulator"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (setf
     (cpu-accumulator c)
     (logand (cpu-accumulator c) val))
    (set-zn c (cpu-accumulator c))
    (format
     nil
     "ANDA with mode ~a from address ~x that holds value ~x. Produces value ~x"
     mode addr val (cpu-accumulator c))))

; (defun bit (c inst)
;  "BIT: and value with accumulator, don't store."
;  (let ((mode (instruction-addressing-mode inst))
;        (val (get-value c inst))
;        (addr (get-address c inst)))
;    (set-zn c (logand val (cpu-accumulator c)))
;    (format
;     nil
;     "BIT with mode ~a from address ~x that holds value ~x"
;     mode addr val)))

(defun cmp (c inst)
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (set-zn c (wrap-byte (- (cpu-accumulator c) val)))
    (setf
     (flags-carry (cpu-sr c))
     (>= (cpu-accumulator c) val))
    (format
     nil
     "CMP with mode ~a from address ~x that holds value ~x"
     mode addr val)))

(defun cpy (c inst)
 (let ((mode (instruction-addressing-mode inst))
       (val (get-value c inst))
       (addr (get-address c inst)))
   (set-zn c (wrap-byte (- (cpu-y c) val)))
   (setf
    (flags-carry (cpu-sr c))
    (>= (cpu-y c) val))
   (format
    nil
    "CPY with mode ~a from address ~x that holds value ~x"
    mode addr val)))

(defun cpx (c inst)
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (set-zn c (wrap-byte (- (cpu-x c) val)))
    (setf
     (flags-carry (cpu-sr c))
     (>= (cpu-x c) val))
    (format
     nil
     "CPX with mode ~a from address ~x that holds value ~x"
     mode addr val)))

(defun dey (c inst)
  "DEY: Decrement y register"
  (declare (ignore inst))
  (setf
   (cpu-y c)
   (wrap-byte (- (cpu-y c) 1)))
  (set-zn c (wrap-byte (cpu-y c)))
  (format
   nil
   "DEY. Decremented cpu-y to ~x"
   (cpu-y c)))

(defun dex (c inst)
  "DEY: Decrement y register"
  (declare (ignore inst))
  (setf
   (cpu-x c)
   (wrap-byte (- (cpu-x c) 1)))
  (set-zn c (wrap-byte (cpu-x c)))
  (format
   nil
   "DEX. Decremented cpu-x to ~x"
   (cpu-x c)))

(defun inc (c inst)
 (let ((val (get-value c inst))
       (addr (get-address c inst)))
   (set-zn c (write-cpu c addr (wrap-byte (1+ val))))
   (format
    nil
    "INC, now holds ~x." (wrap-byte (1+ val)))))

(defun iny (c inst)
  (declare (ignore inst))
  (set-zn
   c
   (setf
    (cpu-y c)
    (wrap-byte (1+ (cpu-y c)))))
  (format
   nil
   "INY, now holds ~x." (cpu-y c)))
