(in-package :6502-cpu)
(declaim (optimize (speed 3) (safety 1)))
(defun brk (c inst)
  (declare (cpu c) (instruction inst))
  "BRK: cause nmi"
  (declare (ignore inst))
  (push16 c (wrap-word (1+ (cpu-pc c))))
  (php c nil)
  (sei c nil)
  (setf (cpu-pc c) (make-word-from-bytes (read-cpu c #xFFFF) (read-cpu c #xFFFE))))

(defun rti (c inst)
  (declare (cpu c) (instruction inst))
  "Return from interrupt"
  (declare (ignore inst))
  (setf
   (cpu-sr c)
   (make-flags-from-byte (logior #x20 (logand (pull-stack c) #xEF))))
  (setf (cpu-pc c) (pull16 c)))

(defun jsr (c inst)
  (declare (cpu c) (instruction inst))
  "JSR: jump subroutine"
  (push16 c (wrap-word (- (cpu-pc c) 1)))
  (setf (cpu-pc c) (get-address c inst)))

(defun jmp-absolute (c inst)
  (declare (cpu c) (instruction inst))
  (setf (cpu-pc c) (get-address c inst)))

(defun jmp-indirect (c inst)
  (declare (cpu c) (instruction inst))
  (setf (cpu-pc c) (get-address c inst)))

(defun rts (c inst)
  (declare (cpu c) (instruction inst))
  (declare (ignore inst))
  (setf (cpu-pc c) (wrap-word (1+ (the (unsigned-byte 16) (pull16 c))))))

(defun bpl (c inst)
  (declare (cpu c) (instruction inst))
  (when (not (flags-negative (cpu-sr c)))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))

(defun bmi (c inst)
  (declare (cpu c) (instruction inst))
  (when (flags-negative (cpu-sr c))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))

(defun bcs (c inst)
  (declare (cpu c) (instruction inst))
  (when (flags-carry (cpu-sr c))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))

(defun bvc (c inst)
  (declare (cpu c) (instruction inst))
  (when (not (flags-overflow (cpu-sr c)))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))

(defun bvs (c inst)
  (declare (cpu c) (instruction inst))
  (when (flags-overflow (cpu-sr c))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))

(defun bcc (c inst)
  (declare (cpu c) (instruction inst))
  (when (not (flags-carry (cpu-sr c)))
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))

(defun bne (c inst)
  (declare (cpu c) (instruction inst))
  (when (not (flags-zero (cpu-sr c)))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))


(defun beq (c inst)
  (declare (cpu c) (instruction inst))
  (when (flags-zero (cpu-sr c))
    ;Branch taken means increment cycles
    (incf (cpu-cycles c))
    (setf (cpu-pc c) (get-address c inst))))
