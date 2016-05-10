(in-package :cl-user)

(defpackage #:6502-cpu
  (:nicknames #:cpu)
  (:use :cl :cl-user)
  (:export #:make-cpu #:pages-differ #:reset #:power-on #:pull-stack
           #:push-stack #:pull16 #:push16 #:cpu-cycles #:cpu-accumulator #:cpu-x
           #:cpu-y #:cpu-pc #:cpu-sp #:cpu-memory #:step-pc #:fetch #:wrap-word
           #:wrap-byte #:step-cpu #:decode #:execute #:make-instruction
           #:cpu-memory-get #:cpu-memory-set #:ora #:to-signed-byte-8
           #:trigger-nmi-callback #:trigger-irq-callback))

(in-package :6502-cpu)

(defvar instructions (make-hash-table :test 'equal))

(defvar
  cycles-per-instruction
  (make-array
   256
   :initial-contents
   '(7 6 2 8 3 3 5 5 3 2 2 2 4 4 6 6
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7
     6 6 2 8 3 3 5 5 4 2 2 2 4 4 6 6
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7
     6 6 2 8 3 3 5 5 3 2 2 2 3 4 6 6
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7
     6 6 2 8 3 3 5 5 4 2 2 2 5 4 6 6
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7
     2 6 2 6 3 3 3 3 2 2 2 2 4 4 4 4
     2 6 2 6 4 4 4 4 2 5 2 5 5 5 5 5
     2 6 2 6 3 3 3 3 2 2 2 2 4 4 4 4
     2 5 2 5 4 4 4 4 2 4 2 4 4 4 4 4
     2 6 2 8 3 3 5 5 2 2 2 2 4 4 6 6
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7
     2 6 2 8 3 3 5 5 2 2 2 2 4 4 6 6
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7)))

(defvar
  instruction-page-cycles
  (make-array
   256
   :initial-contents
   '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 1 0 0 0 0 0 1 0 1 1 1 1 1
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 1 0 0 0 0 0 0 0 1 0 0 1 1 0 0)))

(defvar instructions (make-hash-table :test 'equal))

(defstruct flags
  "Flag register"
  (carry nil)
  (zero nil)
  (interrupt nil)
  (bcd nil)
  (soft-interrupt nil)
  (unused T)
  (overflow nil)
  (negative nil))

(defstruct cpu
  "A model 6502"
  (cycles 0)
  (accumulator 0 :type (unsigned-byte 8))
  (x 0 :type (unsigned-byte 8))
  (y 0 :type (unsigned-byte 8))
  (pc 0 :type (unsigned-byte 16))
  (sp 0 :type (unsigned-byte 8))
  (sr (make-flags))
  (memory-get (make-array 6))
  (memory-set (make-array 3))
  (memory (make-array #x800 :element-type '(unsigned-byte 8)))
  (interrupt :none))

(defstruct instruction
  "6502 instruction"
  (unmasked-opcode 0 :type (unsigned-byte 8))
  (opcode 0 :type (unsigned-byte 8))
  (hi-byte 0 :type (unsigned-byte 8))
  (lo-byte 0 :type (unsigned-byte 8))
  (addressing-mode :implicit))

(defun trigger-nmi-callback (c)
  (lambda ()
          (setf (cpu-interrupt c) :nmi)))

(defun trigger-irq-callback (c)
  (lambda ()
          (when (equal :none (cpu-interrupt c))
            (setf (cpu-interrupt c) :irq))))

(defun wrap-byte (val)
  (logand #xFF val))

(defun wrap-word (val)
  (logand #xFFFF val))

(defun make-byte-from-flags (f)
  (wrap-byte
   (logior
    (if (flags-carry f) 1 0)
    (ash (if (flags-zero f) 1 0) 1)
    (ash (if (flags-interrupt f) 1 0) 2)
    (ash (if (flags-bcd f) 1 0) 3)
    (ash (if (flags-soft-interrupt f) 1 0) 4)
    (ash (if (flags-unused f) 1 0) 5)
    (ash (if (flags-overflow f) 1 0) 6)
    (ash (if (flags-negative f) 1 0) 7))))

(defun make-flags-from-byte (val)
  (make-flags
   :carry (= (ldb (byte 1 0) val) 1)
   :interrupt (= (ldb (byte 1 1) val) 1)
   :zero (= (ldb (byte 1 2) val) 1)
   :bcd (= (ldb (byte 1 3) val) 1)
   :soft-interrupt (= (ldb (byte 1 4) val) 1)
   :unused (= (ldb (byte 1 5) val) 1)
   :overflow (= (ldb (byte 1 6) val) 1)
   :negative (= (ldb (byte 1 7) val) 1)))

(defun to-signed-byte-8 (val)
  (if (= (ldb (byte 1 7) val) 1)
    (*
     -1
     (wrap-byte
      (1+ (lognot val))))
    (logand #x7f val)))

(defun make-word-from-bytes (hi lo)
  (wrap-word
   (logior
    (ash hi 8)
    lo)))

(defun pages-differ (a b)
  (declare ((unsigned-byte 16) a b))
  (not
   (=
    (logand a #xFF00)
    (logand b #xFF00))))

;TODO: Give the CPU a console reference
(defun read-cpu (c addr)
  "Reads the memory at the specified address"
  (cond
    ;CPU internal memory
    ((<= addr #x1FFF) (funcall (aref (cpu-memory-get c) 0) addr))
    ;PPU
    ((<= addr #x3FFF) (funcall (aref (cpu-memory-get c) 1) addr))
    ;APU and IO Registers
    ((<= addr #x401F) (funcall (aref (cpu-memory-get c) 1) addr)); THIS IS WRONG, CHANGE IT LATER
    ;Mapper Registers
    ((<= addr #x5FFF) 0);(funcall (aref (cpu-memory-get c) 3) addr))
    ;PRG RAM
    ((<= addr #x7FFF) (funcall (aref (cpu-memory-get c) 4) addr))
    ;PRG ROM
    ((<= addr #xFFFF) (funcall (aref (cpu-memory-get c) 5) addr))))

;TODO: Write functions for this
(defun write-cpu (c addr val)
  (cond
    ;CPU internal memory
    ((<= addr #x1FFF) (funcall (aref (cpu-memory-set c) 0) addr val))
    ;PPU Registers
    ((<= addr #x3FFF) (funcall (aref (cpu-memory-set c) 1) addr val))
    ;PRG RAM
    ((and (<= addr #x7FFF) (>= addr #x6000))
     (funcall (aref (cpu-memory-set c) 2) addr val))
    (T 0)));(format t "We really can't write to 0~x" addr))))

(defun reset (c)
  "Reset state of cpu"
  (setf (cpu-sp c) (wrap-byte (- (cpu-sp c) 3)))
  (setf (flags-interrupt (cpu-sr c)) T))

(defun power-on (c)
  "Power on state of cpu"
  (setf
   (cpu-sr c)
   (make-flags
    :carry nil
    :zero nil
    :interrupt T
    :bcd nil
    :soft-interrupt T
    :unused T
    :overflow nil
    :negative nil))
  (setf (cpu-sp c) #xFD)
  (setf
   (cpu-pc c)
   (wrap-word(make-word-from-bytes (read-cpu c #xFFFD) (read-cpu c #xFFFC)))))

(defun pull-stack (c)
  "Empty stack pull"
  (setf (cpu-sp c) (wrap-byte (1+ (cpu-sp c))))
  (aref (cpu-memory c) (logior (cpu-sp c) #x100)))

(defun push-stack (c val)
  "Put a value on the stack and then push it forwards"
  (declare ((unsigned-byte 8) val))
  (setf
   (aref
    (cpu-memory c)
    (logior (cpu-sp c) #x100))
   val)
  (setf
   (cpu-sp c)
   (wrap-byte (1- (cpu-sp c)))))

(defun pull16 (c)
  "Pull twice and make a 16 bit address."
  (logior (pull-stack c) (ash (pull-stack c) 8)))

(defun push16 (c val)
  "Push twice."
  (push-stack c (wrap-byte (ash val -8)))
  (push-stack c (wrap-byte val)))

(defun step-pc (c inst)
  "Step the pc according to the addressing mode."
  (let ((mode (instruction-addressing-mode inst)))
    (setf
     (cpu-pc c)
     (wrap-word
      (+
       (cpu-pc c)
       (cond
         ((equal mode :implicit) 1)
         ((equal mode :accumulator) 1)
         ((equal mode :immediate) 2)
         ((equal mode :zero-page) 2)
         ((equal mode :absolute) 3)
         ((equal mode :relative) 2)
         ((equal mode :indirect) 3)
         ((equal mode :zero-page-indexed-x) 2)
         ((equal mode :zero-page-indexed-y) 2)
         ((equal mode :absolute-indexed-x) 3)
         ((equal mode :absolute-indexed-y) 3)
         ((equal mode :indexed-indirect) 2)
         ((equal mode :indirect-indexed) 2)
         (T 1))))))) ;Silence warnings with this last line

(defun set-zn (c val)
  "Sets the zero or negative flag"
  ;If zero, set the bit
  (setf
   (flags-zero (cpu-sr c))
   (if (= val 0)
     T
     nil))
  ;If the MSB is set, it's negative.
  (setf
   (flags-negative (cpu-sr c))
   (if (= (ldb (byte 1 7) val) 1)
     T
     nil)))

(defun get-address (c inst)
  "Get the address the instruction is talking about"
  (let ((mode (instruction-addressing-mode inst))
        (lo-byte (instruction-lo-byte inst))
        (hi-byte (instruction-hi-byte inst)))
    (cond
      ;Somewhere in zero page...
      ((equal mode :zero-page) lo-byte)
      ;Super simple, just make a two byte address from the supplied two bytes
      ((equal mode :absolute)
       (make-word-from-bytes hi-byte lo-byte))
      ;Treat the low byte as though it were signed, use it as an offset for PC
      ((equal mode :relative)
       (wrap-word
        (+
         (cpu-pc c)
         ;Perform two's complement to figure out the offset
         (to-signed-byte-8 lo-byte))))
      ;Read the address contained at the supplied two byte address.
      ((equal mode :indirect)
       (let ((ptr-addr (make-word-from-bytes hi-byte lo-byte)))
         (make-word-from-bytes
          (read-cpu c (wrap-word (1+ ptr-addr)))
          (read-cpu c ptr-addr))))
      ;Add the x register to the low-byte for zero-page addressing
      ((equal mode :zero-page-indexed-x)
       (wrap-byte (+ lo-byte (cpu-x c))))
      ;Add the y register to the low-byte for zero-page addressing
      ((equal mode :zero-page-indexed-y)
       (wrap-byte (+ lo-byte (cpu-y c))))
      ;Add the x register to the supplied two byte address
      ((equal mode :absolute-indexed-x)
       (wrap-word
        (+
         (make-word-from-bytes hi-byte lo-byte)
         (cpu-x c))))
      ;Add the y register to the supplied two byte address
      ((equal mode :absolute-indexed-y)
       (wrap-word
        (+
         (make-word-from-bytes hi-byte lo-byte)
         (cpu-y c))))
      ;Get the address contained at lo-byte + x
      ((equal mode :indexed-indirect)
       (if (= lo-byte #xFF)
         (progn
          (make-word-from-bytes
           (read-cpu c #x00)
           (read-cpu c #xFF)))
         (progn
          (make-word-from-bytes
           (read-cpu
            c
            (wrap-byte
             (+ 1 lo-byte (cpu-x c))))
           (read-cpu
            c
            (wrap-byte
             (+ lo-byte (cpu-x c))))))))
      ;Get the address containted at lo-byte + y
      ((equal mode :indirect-indexed)
       (if (= lo-byte #xFF)
         (progn
          (+
           (cpu-y c)
           (make-word-from-bytes
            (read-cpu c #x00)
            (read-cpu c #xFF))))
         (progn
          (+
           (cpu-y c)
           (make-word-from-bytes
            (read-cpu c (+ 1 lo-byte))
            (read-cpu c lo-byte))))))
      (T 0))))

(defun get-value (c inst)
  "Get the value from an instruction"
  (if (equal :immediate (instruction-addressing-mode inst))
    (instruction-lo-byte inst)
    (if (equal :accumulator (instruction-addressing-mode inst))
      (cpu-accumulator c)
      (read-cpu c (get-address c inst)))))

(defun fetch (c)
  "Fetch the next instruction from memory"
  (make-instruction
   :unmasked-opcode (read-cpu c (cpu-pc c))
   :lo-byte (read-cpu c (+ (cpu-pc c) 1))
   :hi-byte (read-cpu c (+ (cpu-pc c) 2))))

(defun determine-addressing-mode (opcode)
  ;We really only care about the opcode as: AAA???CC
  ;BBB is normally just addressing mode, which we store in the intstruction
  (let ((cc (logand opcode #x03))
        (bbb (logand (ash opcode -2) #x07))
        (aaa (logand (ash opcode -5) #x07)))
    (case cc
      (0
       (case bbb
         (0 :immediate)
         (1 :zero-page)
         (3 :absolute)
         (5 :zero-page-indexed-x)
         (7 :absolute-indexed-x)))
      (1
       (case bbb
         (0 :indexed-indirect)
         (1 :zero-page)
         (2 :immediate)
         (3 :absolute)
         (4 :indirect-indexed)
         (5 :zero-page-indexed-x)
         (6 :absolute-indexed-y)
         (7 :absolute-indexed-x)))
      (2
       (case bbb
         (0 :immediate)
         (1 :zero-page)
         (2 :accumulator)
         (3 :absolute)
         (5 (if (member aaa '(4 5)) :zero-page-indexed-y :zero-page-indexed-x))
         (7 (if (= aaa 5) :absolute-indexed-y :absolute-indexed-x))))
      (otherwise (print "Bad opcode")))))

;TODO: Test this somehow...
(defun decode (inst)
  "Decodes the opcode and returns a constructed instruction."
  (let*
    ((opcode (instruction-unmasked-opcode inst))
     (lo-byte (instruction-lo-byte inst))
     (hi-byte (instruction-hi-byte inst))
     (masked-opcode (logand opcode #xE3))
     (addressing-mode (determine-addressing-mode opcode)))
    ;If it is a special case, modify
    (cond
      ;Jump Absolute
      ((= opcode #x4C)
       (progn
        (setf addressing-mode :absolute)
        (setf masked-opcode opcode)))
      ((member
        opcode
        '(#x10 #x30 #x50 #x70 #x90 #xB0 #xD0 #xF0))
       (progn
        (setf addressing-mode :relative)
        (setf masked-opcode opcode)))
      ((member
        opcode
        '(#x0 #x40 #x60))
       (progn
        (setf addressing-mode :implicit)
        (setf masked-opcode opcode)))
      ((member
        opcode
        '(#x20 #x2C))
       (progn
        (setf addressing-mode :absolute)
        (setf masked-opcode opcode)))
      ((= opcode #x6C)
       (progn
        (setf addressing-mode :indirect)
        (setf masked-opcode opcode)))
      ((= opcode #x24)
       (progn
        (setf addressing-mode :zero-page)
        (setf masked-opcode opcode)))
      ((member opcode '(#x0 #x40 #x60))
       (progn
        (setf addressing-mode :implicit)
        (setf masked-opcode opcode)))
      ((member opcode '(#x08 #x28 #x48 #x68 #x88 #xA8 #xC8 #xE8 #x18
                         #x38 #x58 #x78 #x98 #xB8 #xD8 #xF8))
       (progn
        (setf addressing-mode :implicit)
        (setf masked-opcode opcode)))
      ((member opcode '(#x8A #x9A #xAA #xBA #xCA #xEA))
       (progn
        (setf addressing-mode :implicit)
        (setf masked-opcode opcode))))
    ;Make the instruction
    (make-instruction
     :addressing-mode addressing-mode
     :opcode masked-opcode
     :unmasked-opcode opcode
     :hi-byte hi-byte
     :lo-byte lo-byte)))

(defun instruction-cycles (c inst)
  (let* ((address (get-address c inst))
        (mode (instruction-addressing-mode inst))
        (unmasked (instruction-unmasked-opcode inst))
        (page-cycles (aref instruction-page-cycles unmasked)))
    (+
     ;Get the number of cycles as per usual
     (aref cycles-per-instruction unmasked)
     ;Add page-cycles if we crossed a bound
     (cond
       ((equal mode :absolute-indexed-x)
        (if (pages-differ
             address
             (wrap-word (- address (cpu-x c))))
          page-cycles
          0))
       ((equal mode :absolute-indexed-y)
        (if (pages-differ
             address
             (wrap-word (- address (cpu-y c))))
          page-cycles
          0))
       ((equal mode :indirect-indexed)
        (if (pages-differ
             address
             (wrap-word
              (+
               (cpu-x c)
               (make-word-from-bytes
                (instruction-hi-byte inst)
                (instruction-lo-byte inst)))))
          page-cycles
          0))
       ;Other modes do not suffer from this.
       (T 0)))))

(defun execute (c inst)
  (let ((cycles (instruction-cycles c inst))
        (instruction (gethash (instruction-opcode inst) instructions)))
    (if (not (null instruction))
      ;(print (funcall instruction c inst))
      (funcall instruction c inst)
      (progn (print (format nil "Uknown instruction PC: 0x~x Unmasked-opcode 0x~x masked 0x~x" (cpu-pc c) (instruction-unmasked-opcode inst) (instruction-opcode inst))) (sb-ext:exit)))
    (incf (cpu-cycles c) cycles)
    cycles))

(defun nmi (c)
  (push16 c (cpu-pc c))
  (php c nil)
  (setf (cpu-pc c) (make-word-from-bytes (read-cpu c #xFFFB) (read-cpu c #xFFFA)))
  (setf (flags-interrupt (cpu-sr c)) T)
  (incf (cpu-cycles c) 7))

(defun irq (c)
  (push16 c (cpu-pc c))
  (php c nil)
  (setf (cpu-pc c) (make-word-from-bytes (read-cpu c #xFFFF) (read-cpu c #xFFFE)))
  (setf (flags-interrupt (cpu-sr c)) T)
  (incf (cpu-cycles c) 7))

(defun step-cpu (c)
  "Steps the cpu through an instruction, returns the number of cycles it took."
  (case (cpu-interrupt c)
    (:none 0)
    (:irq (irq c))
    (:nmi (nmi c)))
  (setf (cpu-interrupt c) :none)
  (let ((inst (decode (fetch c))))
    ;Remember to step the pc before execution.
    (step-pc c inst)
    (execute c inst)))
