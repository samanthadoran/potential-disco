(in-package :cl-user)

(defpackage #:NES-cartridge
  (:nicknames #:nes-cart)
  (:use :cl :cl-user)
  (:export #:load-cartridge))

(in-package :NES-cartridge)
; type
;   Cartridge* = ref CartridgeObj
;   CartridgeObj = object
;     prgROM*: seq[uint8]
;     prgROMWindow*: uint8
;     prgRAM*: seq[uint8]
;     prgRAMWindow*: uint8
;     chrROM*: seq[uint8]
;     chrRAM*: seq[uint8]
;     chrWindow*: uint8
;     mapperNumber*: int
; const PRGSize: uint32 = 0x4000u32
; const CHRSize: uint32 = 0x2000u32
;(make-array #x4000 :element-type '(unsigned-byte 8))
(defstruct ines-header
  "ines header spec"
  (magic (make-array 4 :element-type '(unsigned-byte 8)))
  ;Size of prg rom in 16 KiB units
  (size-of-prg-rom 0 :type (unsigned-byte 8))
  ;Size of chr rom in 8 KiB units
  (size-of-chr-rom 0 :type (unsigned-byte 8))
  (flags6 0 :type (unsigned-byte 8))
  (flags7 0 :type (unsigned-byte 8))
  ;Size of PRG ram in 8 KiB unites
  (size-of-prg-ram 0 :type (unsigned-byte 8))
  (flags9 0 :type (unsigned-byte 8))
  (flags10 0 :type (unsigned-byte 8))
  (zero-pad (make-array 5 :element-type '(unsigned-byte 8))))

(defstruct cartridge
  "A model NES cartridge"
  (prg-rom 0)
  (prg-rom-window 0 :type (unsigned-byte 8))
  (prg-ram 0)
  (prg-ram-window 0 :type (unsigned-byte 8))
  (chr-rom 0)
  (chr-ram 0)
  (chr-window 0 :type (unsigned-byte 8))
  (mapper-number 0))

(defun load-header (seq)
  (make-ines-header
   :magic (subseq seq 0 4)
   :size-of-prg-rom (aref seq 4)
   :size-of-chr-rom (aref seq 5)
   :flags6 (aref seq 6)
   :flags7 (aref seq 7)
   :size-of-prg-ram (aref seq 8)
   :flags9 (aref seq 9)
   :flags10 (aref seq 10)
   :zero-pad (subseq seq 11 16)))

(defun load-cartridge (filepath)
  (let ((cart (make-cartridge)) (header (make-ines-header)))
    (with-open-file (stream filepath :element-type '(unsigned-byte 8))
      (let ((seq
             (make-array
              (file-length stream)
              :element-type '(unsigned-byte 8))))
        (read-sequence seq stream)
        (setf header (load-header seq))
        ;Skip trainers, god this is hacky
        (if (= (logand (ash (ines-header-flags6 header) -2) 1) 1)
          (read-sequence (make-array 512 :element-type '(unsigned-byte 8)) stream))
        (princ "PRG Rom size: 0x")
        (setq *print-base* 16)
        (princ (* #x4000 (ines-header-size-of-prg-rom header)))))
    cart))
