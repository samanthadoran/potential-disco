(in-package :cl-user)

(defpackage #:NES-console
  (:nicknames #:nes)
  (:use :cl :cl-user :6502-cpu :NES-cartridge :NES-ppu)
  (:export #:make-nes #:console-on #:nes-cpu #:nes-ppu #:nes-cart #:step-nes))

(in-package :NES-console)

(defstruct nes
  "A model nes"
  (cpu (6502-cpu:make-cpu))
  (cart (NES-cartridge:make-cartridge))
  (ppu (NES-ppu:make-ppu)))

(defun cpu-to-cpu-read (n)
  (lambda (addr)
          (aref
           (6502-cpu:cpu-memory (nes:nes-cpu n))
           (mod
            addr
            (array-dimension (6502-cpu:cpu-memory (nes:nes-cpu n)) 0)))))

(defun cpu-to-cpu-write (n)
  (lambda (addr val)
          (setf
           (aref
            (6502-cpu:cpu-memory (nes:nes-cpu n))
            (mod
             addr
             (array-dimension (6502-cpu:cpu-memory (nes:nes-cpu n)) 0)))
           val)))

(defun cpu-to-cart-read (n)
  (lambda (addr)
          (aref
           (NES-cartridge:cartridge-prg-rom (nes-cart n))
           (mod
            addr
            (array-dimension (NES-cartridge:cartridge-prg-rom (nes-cart n)) 0)))))

;TODO: Implement the ppu.
(defun cpu-to-ppu-read (n)
  (lambda (addr)
          ;If oam, don't mod the address
          (if (= addr #x4014)
            (NES-ppu:read-register (nes-ppu n) addr)
            (NES-ppu:read-register (nes-ppu n) (mod addr 8)))))
(defun cpu-to-ppu-write (n)
  (lambda (addr val)
          (if (= addr #x4014)
            (NES-ppu:write-register (nes-ppu n) addr val)
            (NES-ppu:write-register (nes-ppu n) (mod addr 8) val))))

(defun console-on (n)
  (NES-ppu:reset-ppu (nes-ppu n))
  (setf (nes-cart n) (NES-cartridge:load-cartridge #P"/home/samanthadoran/nes/smb.nes"))
  (setf (NES-ppu:ppu-trigger-nmi-callback (nes-ppu n)) (6502-cpu:trigger-nmi-callback (nes-cpu n)))
  (setf
   (aref (6502-cpu:cpu-memory-get (nes-cpu n)) 0)
   (cpu-to-cpu-read n))
  (setf
   (aref (6502-cpu:cpu-memory-set (nes-cpu n)) 0)
   (cpu-to-cpu-write n))
  (setf
   (aref (6502-cpu:cpu-memory-get (nes-cpu n)) 1)
   (cpu-to-ppu-read n))
  (setf
   (aref (6502-cpu:cpu-memory-set (nes-cpu n)) 1)
   (cpu-to-ppu-write n))
  (setf
   (aref (6502-cpu:cpu-memory-get (nes-cpu n)) 5)
   (cpu-to-cart-read n))
  (6502-cpu:power-on (nes-cpu n)))

(defun step-nes (n steps)
  (loop for s from 1 to steps
    do
    (let ((cycles (* 3 (6502-CPU:step-cpu (nes-cpu n)))))
      (loop for i from 1 to cycles
        do
        (NES-ppu:step-ppu (nes-ppu n))))))
