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

(defun load-cartridge (filepath)
  (declare (ignore filepath)))
