(in-package :cl-user)

(defpackage #:6502-cpu
  (:nicknames #:cpu)
  (:use :cl :cl-user)
  (:export #:make-cpu #:reset #:power-on #:cpu-cycles #:cpu-accumulator #:cpu-pc
           #:cpu-memory #:step-pc #:fetch #:step-cpu #:make-instruction
           #:cpu-memory-get #:cpu-memory-set #:to-signed-byte-8 #:read-cpu
           #:trigger-nmi-callback #:trigger-irq-callback #:add-to-stall))

(in-package :6502-cpu)
(declaim (optimize (speed 3) (safety 1)))
(defvar instructions (make-hash-table :test 'equal))

(defvar
  cycles-per-instruction
  (make-array
   256
   :element-type '(unsigned-byte 8)
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
   :element-type '(unsigned-byte 8)
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
  (cycles 0 :type (unsigned-byte 16))
  (stall 0 :type (unsigned-byte 16))
  (accumulator 0 :type (unsigned-byte 8))
  (x 0 :type (unsigned-byte 8))
  (y 0 :type (unsigned-byte 8))
  (pc 0 :type (unsigned-byte 16))
  (sp 0 :type (unsigned-byte 8))
  (sr (make-flags) :type flags)
  (memory-get
   (make-array 6 :element-type 'function :initial-element (lambda ()))
   :type (simple-array function 1))
  (memory-set
   (make-array 3 :element-type 'function :initial-element (lambda ()))
   :type (simple-array function 1))
  (memory
   (make-array #x800 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))
  (interrupt :none))

(defstruct instruction
  "6502 instruction"
  (unmasked-opcode 0 :type (unsigned-byte 8))
  (opcode 0 :type (unsigned-byte 8))
  (hi-byte 0 :type (unsigned-byte 8))
  (lo-byte 0 :type (unsigned-byte 8))
  (addressing-mode :implicit))

(defun add-to-stall (c)
  (lambda (to-add)
          (declare ((unsigned-byte 16) to-add) (cpu c))
          (incf (cpu-stall c) to-add)
          (when (= (mod (cpu-cycles c) 2) 1) (incf (cpu-stall c)))))

(defun trigger-nmi-callback (c)
  (declare (cpu c))
  (lambda ()
          (setf (cpu-interrupt c) :nmi)))

(defun trigger-irq-callback (c)
  (declare (cpu c))
  (lambda ()
          (when (not (flags-interrupt (cpu-sr c)))
            (setf (cpu-interrupt c) :irq))))

(defun wrap-byte (val)
  (declare ((signed-byte 64) val))
  (the (unsigned-byte 8) (logand #xFF val)))

(defun wrap-word (val)
  (declare ((signed-byte 64) val))
  (the (unsigned-byte 16) (logand #xFFFF val)))

(defun make-byte-from-flags (f)
  (declare (flags f))
  (logior
   (if (flags-carry f) 1 0)
   (ash (if (flags-zero f) 1 0) 1)
   (ash (if (flags-interrupt f) 1 0) 2)
   (ash (if (flags-bcd f) 1 0) 3)
   (ash (if (flags-soft-interrupt f) 1 0) 4)
   (ash (if (flags-unused f) 1 0) 5)
   (ash (if (flags-overflow f) 1 0) 6)
   (ash (if (flags-negative f) 1 0) 7)))

(defun make-flags-from-byte (val)
  (declare ((unsigned-byte 8) val))
  (make-flags
   :carry (ldb-test (byte 1 0) val)
   :zero (ldb-test (byte 1 1) val)
   :interrupt (ldb-test (byte 1 2) val)
   :bcd (ldb-test (byte 1 3) val)
   :soft-interrupt (ldb-test (byte 1 4) val)
   :unused (ldb-test (byte 1 5) val)
   :overflow (ldb-test (byte 1 6) val)
   :negative (ldb-test (byte 1 7) val)))

(defun to-signed-byte-8 (val)
  (declare ((unsigned-byte 8) val))
  (if (ldb-test (byte 1 7) val)
    (* -1 (wrap-byte (1+ (lognot val))))
    val))

(defun make-word-from-bytes (hi lo)
  (declare ((unsigned-byte 8) hi lo))
  (the (unsigned-byte 16) (logior (ash hi 8) lo)))

(defun pages-differ (a b)
  (declare ((unsigned-byte 16) a b))
  (not (= (ldb (byte 8 8) a) (ldb (byte 8 8) b))))

(defun read-cpu (c addr)
  (declare (cpu c) ((unsigned-byte 16) addr))
  "Reads the memory at the specified address"
  (cond
    ;CPU internal memory
    ((<= addr #x1FFF) (funcall (aref (cpu-memory-get c) 0) addr))
    ;PPU
    ((<= addr #x3FFF) (funcall (aref (cpu-memory-get c) 1) addr))
    ;APU and IO Registers
    ((<= addr #x401F) (funcall (aref (cpu-memory-get c) 2) addr))
    ;Mapper Registers
    ((<= addr #x5FFF) (progn (print "Reads from cpu to mapper unimplemented....") 0))
    ;SAVE RAM
    ((<= addr #x7FFF) (progn (print "Reads from cpu to save ram unimplemented....") 0))
    ;PRG ROM
    ((<= addr #xFFFF) (funcall (aref (cpu-memory-get c) 5) addr))))

(defun read16 (c addr bug)
  (declare (cpu c) ((unsigned-byte 16) addr))
  "Emulate indirect bugs..."
  (if bug
    (let ((lo (read-cpu c addr))
          (hi (read-cpu c (logior (logand addr #xFF00) (wrap-byte (1+ addr))))))
      (the (unsigned-byte 16) (make-word-from-bytes hi lo)))
    (let ((lo (read-cpu c addr)) (hi (read-cpu c (wrap-word (1+ addr)))))
      (the (unsigned-byte 16) (make-word-from-bytes hi lo)))))

(defun write-cpu (c addr val)
  (declare (cpu c) ((unsigned-byte 16) addr) ((unsigned-byte 8) val))
  (cond
    ;CPU internal memory
    ((<= addr #x1FFF) (funcall (aref (cpu-memory-set c) 0) addr val))
    ;PPU Registers
    ((<= addr #x3FFF) (funcall (aref (cpu-memory-set c) 1) addr val))
    ;Don't forget oam-dma
    ((= addr #x4014) (funcall (aref (cpu-memory-set c) 1) addr val))
    ;APU and IO Registers
    ((<= addr #x401F) (funcall (aref (cpu-memory-set c) 2) addr val))
    ;SAVE RAM
    ((and (<= addr #x7FFF) (>= addr #x6000))
     (progn (print "Writes to save ram unimplemented...") 0))
    (T 0)))

(defun reset (c)
  (declare (cpu c))
  "Reset state of cpu"
  (setf (cpu-sp c) (wrap-byte (- (cpu-sp c) 3)))
  (setf (flags-interrupt (cpu-sr c)) T))

(defun power-on (c)
  (declare (cpu c))
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
   (read16 c #xFFFC nil)))

(defun pull-stack (c)
  (declare (cpu c))
  "Empty stack pull"
  (setf (cpu-sp c) (wrap-byte (1+ (cpu-sp c))))
  (aref (cpu-memory c) (logior (cpu-sp c) #x100)))

(defun push-stack (c val)
  (declare (cpu c) ((unsigned-byte 8) val))
  "Put a value on the stack and then push it forwards"
  (setf (aref (cpu-memory c) (logior (cpu-sp c) #x100)) val)
  (setf (cpu-sp c) (wrap-byte (- (cpu-sp c) 1))))

(defun pull16 (c)
  (declare (cpu c))
  "Pull twice and make a 16 bit address."
  (the (unsigned-byte 16) (logior (pull-stack c) (ash (pull-stack c) 8))))

(defun push16 (c val)
  (declare (cpu c) ((unsigned-byte 16) val))
  "Push twice."
  (push-stack c (ash val -8))
  (push-stack c (wrap-byte val)))

(defun step-pc (c inst)
  (declare (cpu c) (instruction inst))
  "Step the pc according to the addressing mode."
  (let ((mode (instruction-addressing-mode inst)))
    (setf
     (cpu-pc c)
     (wrap-word
      (+
       (cpu-pc c)
       (case mode
         (:implicit 1)
         (:accumulator 1)
         (:immediate 2)
         (:zero-page 2)
         (:absolute 3)
         (:relative 2)
         (:indirect 3)
         (:zero-page-indexed-x 2)
         (:zero-page-indexed-y 2)
         (:absolute-indexed-x 3)
         (:absolute-indexed-y 3)
         (:indexed-indirect 2)
         (:indirect-indexed 2)
         (otherwise 1)))))))

(defun set-zn (c val)
  (declare (cpu c) ((unsigned-byte 8) val))
  "Sets the zero or negative flag"
  ;If zero, set the bit
  (setf (flags-zero (cpu-sr c)) (= val 0))
  ;If the MSB is set, it's negative.
  (setf (flags-negative (cpu-sr c)) (ldb-test (byte 1 7) val)))

(defun get-address (c inst)
  (declare (cpu c) (instruction inst))
  "Get the address the instruction is talking about"
  (let ((mode (instruction-addressing-mode inst))
        (lo-byte (instruction-lo-byte inst))
        (hi-byte (instruction-hi-byte inst)))
    (declare ((unsigned-byte 8) lo-byte hi-byte))
        (case mode
         ;Somewhere in zero page...
         (:zero-page lo-byte)
         ;Super simple, just make a two byte address from the supplied two bytes
         (:absolute (make-word-from-bytes hi-byte lo-byte))
         ;Treat the low byte as though it were signed, use it as an offset for PC
         (:relative (wrap-word (+ (cpu-pc c) (to-signed-byte-8 lo-byte))))
         ;Read the address contained at the supplied two byte address.
         (:indirect
          (let ((ptr-addr (make-word-from-bytes hi-byte lo-byte)))
            (read16 c ptr-addr T)))
         ;Add the x register to the low-byte for zero-page addressing
         (:zero-page-indexed-x (wrap-byte (+ lo-byte (cpu-x c))))
         ;Add the y register to the low-byte for zero-page addressing
         (:zero-page-indexed-y (wrap-byte (+ lo-byte (cpu-y c))))
         ;Add the x register to the supplied two byte address
         (:absolute-indexed-x
          (wrap-word
           (+
            (make-word-from-bytes hi-byte lo-byte)
            (cpu-x c))))
         ;Add the y register to the supplied two byte address
         (:absolute-indexed-y
          (wrap-word
           (+
            (make-word-from-bytes hi-byte lo-byte)
            (cpu-y c))))
         ;Get the address contained at lo-byte + x
         (:indexed-indirect
          (read16 c (wrap-byte (+ lo-byte (cpu-x c))) T))
         ;Get the address containted at lo-byte + y
         (:indirect-indexed
          (wrap-word (+ (cpu-y c) (read16 c lo-byte T))))
         (otherwise 0))))

(defun get-value (c inst)
  (declare (cpu c) (instruction inst))
  "Get the value from an instruction"
  (if (equal :immediate (instruction-addressing-mode inst))
    (instruction-lo-byte inst)
    (if (equal :accumulator (instruction-addressing-mode inst))
      (cpu-accumulator c)
      (read-cpu c (get-address c inst)))))

(defun fetch (c)
  (declare (cpu c))
  "Fetch the next instruction from memory"
  (make-instruction
   :unmasked-opcode (read-cpu c (cpu-pc c))
   :lo-byte (read-cpu c (+ (cpu-pc c) 1))
   :hi-byte (read-cpu c (+ (cpu-pc c) 2))))

(defun determine-addressing-mode (opcode)
  (declare ((unsigned-byte 8) opcode))
  ;We really only care about the opcode as: AAA???CC
  ;BBB is normally just addressing mode, which we store in the intstruction
  (let ((cc (ldb (byte 2 0) opcode))
        (bbb (ldb (byte 3 2) opcode))
        (aaa (ldb (byte 3 5) opcode)))
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
  (declare (instruction inst))
  "Decodes the opcode and returns a constructed instruction."
  (let*
    ((opcode (instruction-unmasked-opcode inst))
     (lo-byte (instruction-lo-byte inst))
     (hi-byte (instruction-hi-byte inst))
     (masked-opcode (logand opcode #xE3))
     (addressing-mode (determine-addressing-mode opcode)))
    ;If it is a special case, modify
    (cond
      ((member opcode '(#x10 #x30 #x50 #x70 #x90 #xB0 #xD0 #xF0))
       (progn
        (setf addressing-mode :relative)
        (setf masked-opcode opcode)))
      ((member opcode '(#x20 #x2C #x4C))
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
      ((member opcode '(#x08 #x28 #x48 #x68 #x88 #xA8 #xC8 #xE8 #x18
                        #x38 #x58 #x78 #x98 #xB8 #xD8 #xF8 #x8A
                        #x9A #xAA #xBA #xCA #xEA #x00 #x40 #x60))
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
  (declare (cpu c) (instruction inst))
  (let* ((address (get-address c inst))
         (mode (instruction-addressing-mode inst))
         (unmasked (instruction-unmasked-opcode inst))
         (page-cycles
          (aref
           (the (simple-array (unsigned-byte 8) 1) instruction-page-cycles)
           unmasked)))
    (declare ((unsigned-byte 16) address)
             ((unsigned-byte 8) unmasked page-cycles)
             (type (simple-array (unsigned-byte 8) 1) cycles-per-instruction))
    (+
     ;Get the number of cycles as per usual
     (aref cycles-per-instruction unmasked)
     ;Add page-cycles if we crossed a bound
     (case mode
       (:absolute-indexed-x
        (if (pages-differ address (wrap-word (- address (cpu-x c))))
          page-cycles
          0))
       (:absolute-indexed-y
        (if (pages-differ address (wrap-word (- address (cpu-y c))))
          page-cycles
          0))
       (:indirect-indexed
        (if (pages-differ
             address
             (wrap-word (- address (cpu-y c))))
          page-cycles
          0))
       (otherwise 0)))))

(defun execute (c inst)
  (declare (cpu c) (instruction inst))
  (let ((cycles (instruction-cycles c inst))
        (instruction (gethash (instruction-opcode inst) instructions)))
    (declare ((unsigned-byte 8) cycles) (function instruction))
    (funcall instruction c inst)
    (setf (cpu-cycles c) (+ cycles (cpu-cycles c)))
    cycles))

(defun nmi (c)
  (declare (cpu c))
  (push16 c (cpu-pc c))
  (php c nil)
  (setf (cpu-pc c) (read16 c #xFFFA nil))
  (setf (flags-interrupt (cpu-sr c)) T)
  (setf (cpu-cycles c) (+ 7 (cpu-cycles c))))

(defun irq (c)
  (declare (cpu c))
  (push16 c (cpu-pc c))
  (php c nil)
  (setf (cpu-pc c) (read16 c #xFFFE nil))
  (setf (flags-interrupt (cpu-sr c)) T)
  (setf (cpu-cycles c) (+ 7 (cpu-cycles c))))

(defun step-cpu (c)
  (declare (cpu c))
  "Steps the cpu through an instruction, returns the number of cycles it took."
  (if (> (cpu-stall c) 0)
    (progn
     (decf (cpu-stall c))
     1)
    (progn
     (case (cpu-interrupt c)
       (:none 0)
       (:irq (irq c))
       (:nmi (nmi c)))
     (setf (cpu-interrupt c) :none)
     (let ((inst (decode (fetch c))))
       ;Remember to step the pc before execution.
       (step-pc c inst)
       (execute c inst)))))
