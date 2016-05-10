(in-package :6502-cpu)

(defun brk (c inst)
  "BRK: cause nmi"
  (declare (ignore inst))
  (push16 c (wrap-word (1+ (cpu-pc c))))
  (php c nil)
  (sei c nil)
  (setf (cpu-pc c) (make-word-from-bytes (read-cpu c #xFFFF) (read-cpu c #xFFFE)))
  (format nil "BRK causing nmi"))

(defun rti (c inst)
  "Return from interrupt"
  (declare (ignore inst))
  (setf
   (cpu-sr c)
   (make-flags-from-byte (logior #x20 (logand (pull-stack c) #xEF))))
  (setf
   (cpu-pc c)
   (pull16 c)))

(defun jsr (c inst)
  "JSR: jump subroutine"
  (let ((mode (instruction-addressing-mode inst))
        (addr (get-address c inst)))
    (push16 c (wrap-word (- (cpu-pc c) 1)))
    (setf
     (cpu-pc c)
     addr)
    (format nil "JSR with mode ~a to address ~x" mode addr)))

(defun jmp-absolute (c inst)
  (let ((addr (get-address c inst)))
    (setf
     (cpu-pc c)
     addr)
    (format
     nil
     "JMP absolute to 0x~x" addr)))

(defun jmp-indirect (c inst)
  (let ((hi (instruction-hi-byte inst))
        (lo (instruction-lo-byte inst))
        (old-pc (cpu-pc c)))
    (setf
     (cpu-pc c)
     (if (= (logand lo #xFF) #xFF)
       (progn
        (let* ((addr-base (make-word-from-bytes hi lo))
               (lo-buggy (read-cpu c addr-base))
               (hi-buggy (read-cpu c (1+ (logand #xFF00 addr-base)))))
          (make-word-from-bytes hi-buggy lo-buggy)))
       (get-address c inst)))
    (format
     nil
     "JMP indirect from ~x to ~x, inst looks like ~x"
     old-pc (cpu-pc c) inst)))

(defun rts (c inst)
  (declare (ignore inst))
  (setf
   (cpu-pc c)
   (wrap-word (1+ (pull16 c))))
  (format
   nil
   "RTS to ~x." (cpu-pc c)))

(defun bpl (c inst)
  (when (not (flags-negative (cpu-sr c)))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf
     (cpu-pc c)
     (get-address c inst)))
  (format
   nil
   "BPL pc is now 0x~x" (cpu-pc c)))

(defun bmi (c inst)
 (when (flags-negative (cpu-sr c))
   ;Branch taken means increment cycles
   (incf (cpu-cycles c))
   (setf
    (cpu-pc c)
    (get-address c inst)))
 (format
  nil
  "BMI pc is now 0x~x" (cpu-pc c)))

(defun bcs (c inst)
 (when (flags-carry (cpu-sr c))
   ;Branch taken means increment cycles
   (incf (cpu-cycles c))
   (setf
    (cpu-pc c)
    (get-address c inst)))
 (format
  nil
  "BCS pc is now 0x~x" (cpu-pc c)))

(defun bcc (c inst)
  (when (not (flags-carry (cpu-sr c)))
    (incf (cpu-cycles c))
    (setf
     (cpu-pc c)
     (get-address c inst)))
  (format
   nil
   "BCC pc is now 0x~x" (cpu-pc c)))

(defun bne (c inst)
 (when (not (flags-zero (cpu-sr c)))
   ;Branch taken means increment cycles
   (incf (cpu-cycles c))
   (setf
    (cpu-pc c)
    (get-address c inst)))
 (format
  nil
  "BNE pc is now 0x~x" (cpu-pc c)))


(defun beq (c inst)
 (when (flags-zero (cpu-sr c))
   ;Branch taken means increment cycles
   (incf (cpu-cycles c))
   (setf
    (cpu-pc c)
    (get-address c inst)))
 (format
  nil
  "BEQ pc is now 0x~x" (cpu-pc c)))
