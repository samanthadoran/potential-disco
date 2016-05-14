(in-package :6502-cpu)

(defun nop (c inst)
  (declare (ignore c inst))
  (format nil "NOP"))

(defun adc (c inst)
  (let ((mode (instruction-addressing-mode inst))
        (a (get-value c inst))
        (b (cpu-accumulator c))
        (carry (if (flags-carry (cpu-sr c)) 1 0))
        (addr (get-address c inst)))
    (setf
     (flags-carry (cpu-sr c))
     (if (> (+ a b carry) 255)
       T nil))

    (set-zn c (setf (cpu-accumulator c) (wrap-byte (+ a b carry))))

    (setf
     (flags-overflow (cpu-sr c))
     (if (and (= (logand #x80 (logxor a b)) 0) (not (= (logand #x80 (logxor a (cpu-accumulator c))) 0)))
       T nil))

    (format
     nil
     "ADC with mode ~a~@[ from 0x~x~] that holds value 0x~x. Produces value 0x~x"
     mode (when (not (equal mode :immediate)) addr) b (cpu-accumulator c))))

(defun sbc (c inst)
 (let ((mode (instruction-addressing-mode inst))
       (a (get-value c inst))
       (b (cpu-accumulator c))
       (carry (if (flags-carry (cpu-sr c)) 1 0))
       (addr (get-address c inst)))
   (setf
    (flags-carry (cpu-sr c))
    (if (>= (- b a (1- carry)) 0)
      T nil))

   (set-zn c (setf (cpu-accumulator c) (wrap-byte (- b a (1- carry)))))

   (setf
    (flags-overflow (cpu-sr c))
    (if (and (= (logand #x80 (logxor a b)) 0) (not (= (logand #x80 (logxor a (cpu-accumulator c))) 0)))
      T nil))

   (format
    nil
    "SBC with mode ~a~@[ from 0x~x~] that holds value 0x~x. Produces value 0x~x"
    mode (when (not (equal mode :immediate)) addr) b (cpu-accumulator c))))

(defun asl (c inst)
  "ASL: Shift left one bit"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))

    (setf (flags-carry (cpu-sr c)) (= 1 (ldb (byte 1 7) val)))
    (set-zn
     c
     (if (equal mode :accumulator)
       (progn
        (setf (cpu-accumulator c) (wrap-byte (ash val 1))))
       (write-cpu c addr (wrap-byte (ash val 1)))))
  (format
   nil
   "ASL with mode ~a~@[ from 0x~x~] that holds value 0x~x"
   mode (when (not (equal mode :immediate)) addr) val)))

(defun lsr (c inst)
  "LSR:Shift one bit right"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))

    (setf (flags-carry (cpu-sr c)) (= (logand val 1) 1))

    (set-zn
     c
     (if (equal mode :accumulator)
       (progn
        (setf (cpu-accumulator c) (wrap-byte (ash val -1))))
       (write-cpu c addr (wrap-byte (ash val -1)))))
  (format
   nil
   "LSR with mode ~a~@[ from 0x~x~] that holds value 0x~x"
   mode (when (not (equal mode :immediate)) addr) val)))

(defun rol (c inst)
  "ROL: Rotate all bits left"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst))
        (carry (if (flags-carry (cpu-sr c)) 1 0)))
    (setf
     (flags-carry (cpu-sr c))
     (if (= 1 (ldb (byte 1 7) val))
       T nil))
    (set-zn
     c
     (if (equal mode :accumulator)
      (setf
       (cpu-accumulator c)
       (wrap-byte (logior carry (ash val 1))))
      (write-cpu c addr (wrap-byte (logior carry (ash val 1))))))
    (format
     nil
     "ROL with mode ~a~@[ from 0x~x~] that holds value 0x~x"
     mode (when (not (equal mode :immediate)) addr) val)))

(defun ror (c inst)
 "ROR: Rotate all bits rights"
 (let ((mode (instruction-addressing-mode inst))
       (val (get-value c inst))
       (addr (get-address c inst))
       (carry (if (flags-carry (cpu-sr c)) 128 0)))
   (setf
    (flags-carry (cpu-sr c))
    (if (= 1 (ldb (byte 1 0) val))
      T nil))
   (set-zn
    c
    (if (equal mode :accumulator)
     (setf
      (cpu-accumulator c)
      (wrap-byte (logior carry (ash val -1))))
     (write-cpu c addr (wrap-byte (logior carry (ash val -1))))))
   (format
    nil
    "ROR with mode ~a~@[ from 0x~x~] that holds value 0x~x"
    mode (when (not (equal mode :immediate)) addr) val)))

(defun ora (c inst)
  "ORA: or value with accumulator"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (set-zn c (setf (cpu-accumulator c) (logior (cpu-accumulator c) val)))
  (format
   nil
   "ORA with mode ~a~@[ from 0x~x~] that holds value 0x~x. Produces value 0x~x"
   mode (when (not (equal mode :immediate)) addr) val (cpu-accumulator c))))

(defun eor (c inst)
  "EOR: xor with accumulator"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (set-zn c (setf (cpu-accumulator c) (logxor (cpu-accumulator c) val)))
    (format
     nil
     "EOR with mode ~a~@[ from 0x~x~] that holds value 0x~x. Produces value 0x~x"
     mode (when (not (equal mode :immediate)) addr) val (cpu-accumulator c))))

(defun anda (c inst)
  "ANDA: and value with accumulator"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (set-zn c (setf (cpu-accumulator c) (logand (cpu-accumulator c) val)))
    (format
     nil
     "ANDA with mode ~a~@[ from 0x~x~] that holds value 0x~x. Produces value 0x~x"
     mode (when (not (equal mode :immediate)) addr) val (cpu-accumulator c))))

(defun bit-shadow (c inst)
 "BIT: and value with accumulator, don't store."
 (let ((mode (instruction-addressing-mode inst))
       (val (get-value c inst))
       (addr (get-address c inst)))
   (set-zn c (logand val (cpu-accumulator c)))
   (setf (flags-negative (cpu-sr c)) (logand 1 (ash val -7)))
   (setf (flags-overflow (cpu-sr c)) (logand 1 (ash val -6)))
   (format
    nil
    "BIT with mode ~a from address 0x~x that holds value 0x~x"
    mode addr val)))

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
     "CMP with mode ~a~@[ from 0x~x~] that holds value 0x~x"
     mode (when (not (equal mode :immediate)) addr) val)))

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
    "CPY with mode ~a~@[ from 0x~x~] that holds value 0x~x"
    mode (when (not (equal mode :immediate)) addr) val)))

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
     "CPX with mode ~a~@[ from 0x~x~] that holds value 0x~x"
     mode (when (not (equal mode :immediate)) addr) val)))

(defun dey (c inst)
  "DEY: Decrement y register"
  (declare (ignore inst))
  (set-zn c (setf (cpu-y c) (wrap-byte (- (cpu-y c) 1))))
  (format
   nil
   "DEY. Decremented cpu-y to 0x~x"
   (cpu-y c)))

(defun dex (c inst)
  "DEY: Decrement y register"
  (declare (ignore inst))
  (set-zn c (setf (cpu-x c) (wrap-byte (- (cpu-x c) 1))))
  (format
   nil
   "DEX. Decremented cpu-x to 0x~x"
   (cpu-x c)))

(defun dec (c inst)
  (let ((val (get-value c inst))
        (addr (get-address c inst)))
    (set-zn c (write-cpu c addr (wrap-byte (- val 1))))
    (format
     nil
     "DEC, now holds 0x~x." (wrap-byte (- val 1)))))

(defun inc (c inst)
 (let ((val (get-value c inst))
       (addr (get-address c inst)))
   (set-zn c (write-cpu c addr (wrap-byte (1+ val))))
   (format
    nil
    "INC, now holds 0x~x." (wrap-byte (1+ val)))))

(defun inx (c inst)
  (declare (ignore inst))
  (set-zn c (setf (cpu-x c) (wrap-byte (1+ (cpu-x c)))))
  (format
   nil
   "INX, now holds 0x~x." (cpu-x c)))

(defun iny (c inst)
  (declare (ignore inst))
  (set-zn c (setf (cpu-y c) (wrap-byte (1+ (cpu-y c)))))
  (format
   nil
   "INY, now holds 0x~x." (cpu-y c)))
