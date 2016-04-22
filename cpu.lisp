(in-package :cl-user)

(defpackage #:6502-cpu
  (:nicknames #:cpu)
  (:use :cl :cl-user)
  (:export #:make-cpu #:pages-differ #:reset #:power-on #:pull-stack
           #:push-stack #:pull16 #:push16 #:cpu-cycles #:cpu-accumulator #:cpu-x
           #:cpu-y #:cpu-pc #:cpu-sp #:cpu-memory #:step-pc #:fetch #:wrap-word
           #:wrap-byte #:step-cpu #:decode #:execute #:fetch #:make-instruction))

(in-package :6502-cpu)

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
  (accumulator 0 :type (unsigned-byte 8))
  (x 0 :type (unsigned-byte 8))
  (y 0 :type (unsigned-byte 8))
  (pc 0 :type (unsigned-byte 16))
  (sp 0 :type (unsigned-byte 8))
  (sr (make-flags))
  (memory (make-array #x800 :element-type '(unsigned-byte 8))))

(defstruct instruction
  "6502 instruction"
  (unmasked-opcode 0 :type (unsigned-byte 8))
  (opcode 0 :type (unsigned-byte 8))
  (hi-byte 0 :type (unsigned-byte 8))
  (lo-byte 0 :type (unsigned-byte 8))
  (addressing-mode :implicit))

(defun wrap-byte (val)
  (logand #xFF val))

(defun wrap-word (val)
  (logand #xFFFF val))

(defun make-word-from-bytes (hi lo)
  (wrap-word
   (logior
    (ash hi 8)
    (lo))))

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
    ((< addr #x1FFF)
     (aref
      (cpu-memory c)
      (mod
       addr
       (array-dimension (cpu-memory c) 0))))
    ;PPU
    ((<= addr #x3FFF) 0)
    ;APU and IO Registers
    ((<= addr #x401F) 0)
    ;Mapper Registers
    ((<= addr #x5FFF) 0)
    ;PRG RAM
    ((<= addr #x7FFF) 0)
    ;PRG ROM
    ((<= addr #xFFFF) 0)))

(defun write-cpu (c addr val)
  (cond
    ;CPU internal memory
    ((<= addr #x1FFF)
     (setf
      (aref
       (cpu-memory c)
       (mod
        addr
        (array-dimension (cpu-memory c) 0)))
      val))
    ;PPU Registers
    ((<= addr #x3FFF) 0)
    ;PRG RAM
    ((and (<= addr #x7FFF) (>= addr #x6000)) 0)
    ;Base case
    (T (print "We can't write here! =("))))

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
  (setf (cpu-sp c) #xFD))

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
   (if (ldb (byte 1 7) val)
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
         (if (= (ldb (byte 1 7) lo-byte) 1)
           (*
            -1
            (wrap-byte
             (1+ (lognot (logand #x7f lo-byte)))))
           (logand #x7f lo-byte)))))
      ;Read the address contained at the supplied two byte address.
      ((equal mode :indirect)
       (let ((ptr-addr (make-word-from-bytes hi-byte lo-byte)))
         (make-word-from-bytes
          (read-cpu c (wrap-word (+1 ptr-addr)))
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
       (read-cpu
        c
        (wrap-byte
         (+ lo-byte (cpu-x c)))))
      ;Get the address containted at lo-byte + y
      ((equal mode :indirect-indexed)
       (wrap-byte
        (+
         (read-cpu c lo-byte)
         (cpu-y c))))
      (T 1))))

(defun get-value (c inst)
  "Get the value from an instruction"
  (if (equal :immediate (instruction-addressing-mode inst))
    (instruction-lo-byte inst)
    (read-cpu c (get-address c inst))))

(defun fetch (c)
  "Fetch the next instruction from memory"
  (make-instruction
   :unmasked-opcode (aref (cpu-memory c) (cpu-pc c))
   :lo-byte (aref (cpu-memory c) (wrap-word (+ (cpu-pc c) 1)))
   :hi-byte (aref (cpu-memory c) (wrap-word (+ (cpu-pc c) 2)))))

;TODO: Test this somehow...
(defun decode (inst)
  "Decodes the opcode and returns a constructed instruction."
  (let*
    ((opcode (instruction-unmasked-opcode inst))
     (lo-byte (instruction-lo-byte inst))
     (hi-byte (instruction-hi-byte inst))
     (cc (logand opcode #x03))
     (bbb (logand (ash opcode -2) #x07))
     (aaa (logand (ash opcode -5) #x07))
     ;We really only care about the opcode as: AAA???CC
     ;BBB is normally just addressing mode, which we store in the intstruction
     (masked-opcode (logand opcode #xE3)))
    (cond
      ((= cc 0)
       (cond
         ;Branching
         ((member opcode '(#x10 #x30 #x50 #x70 #x90 #xB0 #xD0 #xF0))
          (make-instruction
           :addressing-mode :relative
           :opcode opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ;BRK, RTI, RTS, interrupt and subroutines
         ((member opcode '(#x0 #x40 #x60))
          (make-instruction
           :addressing-mode :implicit
           :opcode opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ;JSR ABS and BIT ABS
         ((member opcode '(#x20 #x2C))
          (make-instruction
           :addressing-mode :absolute
           :opcode opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ;BIT ZP
         ((= opcode #x24)
          (make-instruction
           :addressing-mode :zero-page
           :opcode opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ;JMP Indirect, emulate the bug!
         ((= opcode #x6C)
          (make-instruction
           :addressing-mode :indirect
           :opcode opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ;A bunch of single byte instructions that follow no pattern
         ((member opcode '(#x08 #x28 #x48 #x68 #x88 #xA8 #xC8 #xE8 #x18
                            #x38 #x58 #x78 #x98 #xB8 #xD8 #xF8))
          (make-instruction
           :addressing-mode :implicit
           :opcode opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         (T (cond
           ((= bbb 0)
            (make-instruction
             :addressing-mode :immediate
             :opcode masked-opcode
             :unmasked-opcode opcode
             :hi-byte hi-byte
             :lo-byte lo-byte))
           ((= bbb 1)
            (make-instruction
             :addressing-mode :zero-page
             :opcode masked-opcode
             :unmasked-opcode opcode
             :hi-byte hi-byte
             :lo-byte lo-byte))
           ((= bbb 3)
            (make-instruction
             :addressing-mode :absolute
             :opcode masked-opcode
             :unmasked-opcode opcode
             :hi-byte hi-byte
             :lo-byte lo-byte))
           ((= bbb 5)
            (make-instruction
             :addressing-mode :zero-page-indexed-x
             :opcode masked-opcode
             :unmasked-opcode opcode
             :hi-byte hi-byte
             :lo-byte lo-byte))
           ((= bbb 7)
            (make-instruction
             :addressing-mode :absolute-indexed-x
             :opcode masked-opcode
             :unmasked-opcode opcode
             :hi-byte hi-byte
             :lo-byte lo-byte))
           (T (print "BAD OP! Got to default case in cc=0"))))))
      ((= cc 1)
       (cond
         ((= bbb 0)
          (make-instruction
           :addressing-mode :indexed-indirect
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 1)
          (make-instruction
           :addressing-mode :zero-page
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 2)
          (make-instruction
           :addressing-mode :immediate
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 3)
          (make-instruction
           :addressing-mode :absolute
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 4)
          (make-instruction
           :addressing-mode :indirect-indexed
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 5)
          (make-instruction
           :addressing-mode :zero-page-indexed-x
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 6)
          (make-instruction
           :addressing-mode :absolute-indexed-y
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         ((= bbb 7)
          (make-instruction
           :addressing-mode :absolute-indexed-x
           :opcode masked-opcode
           :unmasked-opcode opcode
           :hi-byte hi-byte
           :lo-byte lo-byte))
         (T (print "BAD OP! Got to default case in cc=1"))))
      ((= cc 2)
       ;Yet more special case ops
       (if (member opcode '(#x8A #x9A #xAA #xBA #xCA #xEA))
         (make-instruction
          :addressing-mode :implicit
          :opcode opcode
          :unmasked-opcode opcode
          :hi-byte hi-byte
          :lo-byte lo-byte)
         (cond
           ((= bbb 0)
           (make-instruction
            :addressing-mode :immediate
            :opcode masked-opcode
            :unmasked-opcode opcode
            :hi-byte hi-byte
            :lo-byte lo-byte))
           ((= bbb 1)
           (make-instruction
            :addressing-mode :zero-page
            :opcode masked-opcode
            :unmasked-opcode opcode
            :hi-byte hi-byte
            :lo-byte lo-byte))
           ((= bbb 2)
           (make-instruction
            :addressing-mode :accumulator
            :opcode masked-opcode
            :unmasked-opcode opcode
            :hi-byte hi-byte
            :lo-byte lo-byte))
           ((= bbb 3)
           (make-instruction
            :addressing-mode :absolute
            :opcode masked-opcode
            :unmasked-opcode opcode
            :hi-byte hi-byte
            :lo-byte lo-byte))
           ((= bbb 5)
            (if (member aaa '(4 5))
              (make-instruction
               :addressing-mode :zero-page-indexed-y
               :opcode masked-opcode
               :unmasked-opcode opcode
               :hi-byte hi-byte
               :lo-byte lo-byte)
              (make-instruction
               :addressing-mode :zero-page-indexed-x
               :opcode masked-opcode
               :unmasked-opcode opcode
               :hi-byte hi-byte
               :lo-byte lo-byte)))
           ((= bbb 7)
            (if (= aaa 5)
              (make-instruction
               :addressing-mode :absolute-indexed-y
               :opcode masked-opcode
               :unmasked-opcode opcode
               :hi-byte hi-byte
               :lo-byte lo-byte)
              (make-instruction
               :addressing-mode :absolute-indexed-x
               :opcode masked-opcode
               :unmasked-opcode opcode
               :hi-byte hi-byte
               :lo-byte lo-byte)))
           (T (print "BAD OP! Got to default case in cc=2")))))
      (T (print "This shouldn't happen. BAD OP!")))))

(defun execute (c inst)
  (declare (ignore c inst))
  0)

(defun step-cpu (c)
  "Steps the cpu through an instruction, returns the number of cycles it took."
  (let ((inst (decode (fetch c))))
    ;Remember to step the pc before execution.
    (step-pc c inst)
    (execute c inst)))
