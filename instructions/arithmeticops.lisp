(in-package :6502-cpu)
(declaim (optimize (speed 3) (safety 1)))
(defun nop (c inst)
  (declare (cpu c) (instruction inst) (ignore c inst)))

(defun adc (c inst)
  (declare (cpu c) (instruction inst))
  (let* ((a (cpu-accumulator c))
        (b (get-value c inst))
        (carry (if (flags-carry (cpu-sr c)) 1 0))
        (result (+ a b carry)))
    (declare ((unsigned-byte 8) a b carry))

    (set-zn c (setf (cpu-accumulator c) (wrap-byte result)))
    (setf (flags-carry (cpu-sr c)) (> result 255))
    (setf
     (flags-overflow (cpu-sr c))
     (and (= (logand #x80 (logxor a b)) 0) (not (= (logand #x80 (logxor a (cpu-accumulator c))) 0))))))

(defun sbc (c inst)
  (declare (cpu c) (instruction inst))
  (let* ((a (cpu-accumulator c))
        (b (get-value c inst))
        (carry (if (flags-carry (cpu-sr c)) 1 0))
        (result (- a b (- 1 carry))))
    (declare ((unsigned-byte 8) a b carry))

    (set-zn c (setf (cpu-accumulator c) (wrap-byte result)))
    (setf (flags-carry (cpu-sr c)) (>= result 0))
    (setf
     (flags-overflow (cpu-sr c))
     (and (not (= (logand #x80 (logxor a b)) 0)) (not (= (logand #x80 (logxor a (cpu-accumulator c))) 0))))))

(defun asl (c inst)
  (declare (cpu c) (instruction inst))
  "ASL: Shift left one bit"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (declare ((unsigned-byte 8) val) ((unsigned-byte 16) addr))

    (setf (flags-carry (cpu-sr c)) (ldb-test (byte 1 7) val))
    (set-zn
     c
     (if (equal mode :accumulator)
       (setf (cpu-accumulator c) (wrap-byte (ash val 1)))
       (write-cpu c addr (wrap-byte (ash val 1)))))))

(defun lsr (c inst)
  (declare (cpu c) (instruction inst))
  "LSR:Shift one bit right"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (declare ((unsigned-byte 8) val) ((unsigned-byte 16) addr))

    (setf (flags-carry (cpu-sr c)) (ldb-test (byte 1 0) val))
    (set-zn
     c
     (if (equal mode :accumulator)
       (setf (cpu-accumulator c) (wrap-byte (ash val -1)))
       (write-cpu c addr (wrap-byte (ash val -1)))))))

(defun rol (c inst)
  (declare (cpu c) (instruction inst))
  "ROL: Rotate all bits left"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst))
        (carry (if (flags-carry (cpu-sr c)) 1 0)))
    (declare ((unsigned-byte 8) val carry) ((unsigned-byte 16) addr))

    (setf (flags-carry (cpu-sr c)) (ldb-test (byte 1 7) val))
    (set-zn
     c
     (if (equal mode :accumulator)
      (setf (cpu-accumulator c) (wrap-byte (logior carry (ash val 1))))
      (write-cpu c addr (wrap-byte (logior carry (ash val 1))))))))

(defun ror (c inst)
  (declare (cpu c) (instruction inst))
  "ROR: Rotate all bits rights"
  (let ((mode (instruction-addressing-mode inst))
       (val (get-value c inst))
       (addr (get-address c inst))
       (carry (if (flags-carry (cpu-sr c)) 128 0)))
    (declare ((unsigned-byte 8) carry val) ((unsigned-byte 16) addr))

    (setf (flags-carry (cpu-sr c)) (ldb-test (byte 1 0) val))
    (set-zn
     c
     (if (equal mode :accumulator)
       (setf (cpu-accumulator c) (logior carry (ash val -1)))
       (write-cpu c addr (logior carry (ash val -1)))))))

(defun ora (c inst)
  (declare (cpu c) (instruction inst))
  "ORA: or value with accumulator"
  (set-zn
   c
   (setf
    (cpu-accumulator c)
    (logior (cpu-accumulator c) (the (unsigned-byte 8) (get-value c inst))))))

(defun eor (c inst)
  (declare (cpu c) (instruction inst))
  "EOR: xor with accumulator"
  (set-zn
   c
   (setf
    (cpu-accumulator c)
    (logxor (cpu-accumulator c) (the (unsigned-byte 8) (get-value c inst))))))

(defun anda (c inst)
  (declare (cpu c) (instruction inst))
  "ANDA: and value with accumulator"
  (set-zn
   c
   (setf
    (cpu-accumulator c)
    (logand (cpu-accumulator c) (the (unsigned-byte 8) (get-value c inst))))))

(defun bit-shadow (c inst)
  (declare (cpu c) (instruction inst))
  "BIT: and value with accumulator, don't store."
  (let ((val (get-value c inst)))
    (declare ((unsigned-byte 8) val))

    (set-zn c (logand val (cpu-accumulator c)))
    (setf (flags-negative (cpu-sr c)) (ldb-test (byte 1 7) val))
    (setf (flags-overflow (cpu-sr c)) (ldb-test (byte 1 6) val))))

(defun cmp (c inst)
  (declare (cpu c) (instruction inst))
  (let ((val (get-value c inst)))
    (declare ((unsigned-byte 8) val))
    (setf (flags-carry (cpu-sr c)) (>= (cpu-accumulator c) val))
    (set-zn c (wrap-byte (- (cpu-accumulator c) val)))))

(defun cpy (c inst)
  (declare (cpu c) (instruction inst))
  (let ((val (get-value c inst)))
    (declare ((unsigned-byte 8) val))
    (setf (flags-carry (cpu-sr c)) (>= (cpu-y c) val))
    (set-zn c (wrap-byte (- (cpu-y c) val)))))

(defun cpx (c inst)
  (declare (cpu c) (instruction inst))
  (let ((val (get-value c inst)))
    (declare ((unsigned-byte 8) val))
    (setf (flags-carry (cpu-sr c)) (>= (cpu-x c) val))
    (set-zn c (wrap-byte (- (cpu-x c) val)))))

(defun dey (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "DEY: Decrement y register"
  (set-zn c (setf (cpu-y c) (wrap-byte (- (cpu-y c) 1)))))

(defun dex (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  "DEY: Decrement y register"
  (set-zn c (setf (cpu-x c) (wrap-byte (- (cpu-x c) 1)))))

(defun dec (c inst)
  (declare (cpu c) (instruction inst))
  (let ((val (get-value c inst))
        (addr (get-address c inst)))
        (declare ((unsigned-byte 8) val) ((unsigned-byte 16) addr))
    (set-zn c (write-cpu c addr (wrap-byte (- val 1))))))

(defun inc (c inst)
  (declare (cpu c) (instruction inst))
  (let ((val (get-value c inst))
        (addr (get-address c inst)))
    (declare ((unsigned-byte 8) val) ((unsigned-byte 16) addr))
    (set-zn c (write-cpu c addr (wrap-byte (1+ val))))))

(defun inx (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  (set-zn c (setf (cpu-x c) (wrap-byte (1+ (cpu-x c))))))

(defun iny (c inst)
  (declare (cpu c) (instruction inst) (ignore inst))
  (set-zn c (setf (cpu-y c) (wrap-byte (1+ (cpu-y c))))))
