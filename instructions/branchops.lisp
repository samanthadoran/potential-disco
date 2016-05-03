(in-package :6502-cpu)

(defun jsr (c inst)
  "JSR: jump subroutine"
  (let ((mode (instruction-addressing-mode inst))
        (addr (get-address c inst)))
    (push16 c (cpu-pc c))
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
     "JMP absolute to ~x" addr)))

(defun jmp-indirect (c inst)
  (let ((hi (instruction-hi-byte inst))
        (lo (instruction-lo-byte inst)))
    (setf
     (cpu-pc c)
     (if (= (logand lo #xFF) #xFF)
       (progn
        (let* ((addr-base (make-word-from-bytes hi lo))
               (lo-buggy (read-cpu c addr-base))
               (hi-buggy (read-cpu c (logand #xFF00 addr-base))))
          (make-word-from-bytes hi-buggy lo-buggy)))
       (get-address c inst)))
    (format
     nil
     "JMP indirect from ~x to ~x"
     (make-word-from-bytes hi lo) (cpu-pc c))))

(defun rts (c inst)
  (declare (ignore inst))
  (setf
   (cpu-pc c)
   (pull16 c))
  (format
   nil
   "RTS to ~x." (cpu-pc c)))
