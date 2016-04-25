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

(defun test (n)
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
   (lambda (addr)
           (aref
               (6502-cpu:cpu-memory (nes-cpu n))
               (mod
                addr
                 (array-dimension (6502-cpu:cpu-memory (nes-cpu n)) 0)))))
  (setf
   (aref (6502-cpu:cpu-memory-get (nes-cpu n)) 5)
   (test n))
  (6502-cpu:power-on (nes-cpu n)))
