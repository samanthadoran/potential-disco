(in-package :cl-user)

(defpackage #:NES-console
  (:nicknames #:nes)
  (:use :cl :cl-user :6502-cpu :NES-cartridge)
  (:export #:make-nes #:console-on #:nes-cpu #:nes-ppu #:nes-cart))

(in-package :NES-console)

(defstruct nes
  "A model nes"
  (cpu (6502-cpu:make-cpu))
  (cart (NES-cartridge:make-cartridge))
  (ppu 0))

(defun cpu-to-cpu-read (n)
  (lambda (addr)
          (aref
           (6502-cpu:cpu-memory (nes:nes-cpu n))
           (mod
            addr
            (array-dimension (6502-cpu:cpu-memory (nes:nes-cpu n)) 0)))))

(defun cpu-to-cart-read (n)
  (lambda (addr)
          (aref
           (NES-cartridge:cartridge-prg-rom (nes-cart n))
           (mod
            addr
            (array-dimension (NES-cartridge:cartridge-prg-rom (nes-cart n)) 0)))))

(defun console-on (n)
  (setf (nes-cart n) (NES-cartridge:load-cartridge #P"/home/samanthadoran/nes/smb.nes"))
  (setf
   (aref (6502-cpu:cpu-memory-get (nes-cpu n)) 0)
   (cpu-to-cpu-read n))
  (setf
   (aref (6502-cpu:cpu-memory-get (nes-cpu n)) 5)
   (cpu-to-cart-read n))
  (6502-cpu:power-on (nes-cpu n)))
