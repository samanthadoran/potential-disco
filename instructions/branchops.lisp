(defun jsr (c inst)
  "JSR: jump subroutine"
  (let ((mode (instruction-addressing-mode inst))
        (val (get-value c inst))
        (addr (get-address c inst)))
    (push16 c (cpu-pc c))
    (setf
     (cpu-pc c)
     addr)
    (format nil "JSR with mode ~a to address ~x" mode addr)))
