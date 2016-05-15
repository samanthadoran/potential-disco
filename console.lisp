(in-package :cl-user)

(defpackage #:NES-console
  (:nicknames #:nes)
  (:use :cl :cl-user :6502-cpu :NES-cartridge :NES-ppu)
  (:export #:make-nes #:console-on #:nes-cpu #:nes-ppu #:nes-cart #:step-nes
           #:step-frame #:setup-and-emulate))

(in-package :NES-console)
(declaim (optimize (speed 3) (safety 1)))
(defstruct nes
  "A model nes"
  (cpu (6502-cpu:make-cpu))
  (cart (NES-cartridge:make-cartridge))
  (ppu (NES-ppu:make-ppu)))

(defvar mirror-lookup (make-array '(5 4) :element-type '(unsigned-byte 2) :initial-contents '( (0 0 1 1) (0 1 0 1) (0 0 0 0) (1 1 1 1) (0 1 2 3))))

(defun mirror-address (mode addr)
  (declare ((unsigned-byte 16) addr))
  (let* ((address (mod (- addr #x2000) #x1000))
        (table (floor address #x0400))
        (offset (mod address #x0400)))
    (declare ((unsigned-byte 16) address table offset))
    (logand #xFFFF (+ #x2000 offset (* #x0400 (the (unsigned-byte 3) (aref mirror-lookup mode table)))))))

(defun ppu-to-name-table-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (aref
           (the (simple-array (unsigned-byte 8) 1) (NES-ppu:ppu-name-table-data (nes-ppu n)))
           (mod (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) 2048))))

(defun ppu-to-name-table-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1) (NES-ppu:ppu-name-table-data (nes-ppu n)))
            (mod (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) 2048))
           val)))

(defun ppu-to-palette-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (NES-ppu:read-palette (nes-ppu n) (mod addr 32))))

(defun ppu-to-palette-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (NES-ppu:write-palette (nes-ppu n) (mod addr 32) val)))

(defun ppu-to-mapper-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (aref
            (if (arrayp (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-rom (nes-cart n))))
           addr)))

(defun ppu-to-mapper-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (setf
           (aref
            (if (arrayp (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-rom (nes-cart n))))
            addr)
           val)))

(defun cpu-to-cpu-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (aref
           (the (simple-array (unsigned-byte 8) 1) (6502-cpu:cpu-memory (nes:nes-cpu n)))
           (mod
            addr
            #x800))))

(defun cpu-to-cpu-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1) (6502-cpu:cpu-memory (nes:nes-cpu n)))
            (mod
             addr
             #x800))
           val)))

(defun cpu-to-cart-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (aref
           (NES-cartridge:cartridge-prg-rom (nes-cart n))
           (mod
            addr
            (array-dimension (NES-cartridge:cartridge-prg-rom (nes-cart n)) 0)))))

(defun cpu-to-cart-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (setf
           (aref
            (NES-cartridge:cartridge-prg-rom (nes-cart n))
            (mod
             addr
             (array-dimension (NES-cartridge:cartridge-prg-rom (nes-cart n)) 0)))
           val)))

(defun cpu-to-ppu-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          ;If oam, don't mod the address
          (if (= addr #x4014)
            (NES-ppu:read-register (nes-ppu n) addr)
            (NES-ppu:read-register (nes-ppu n) (mod addr 8)))))
(defun cpu-to-ppu-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (if (= addr #x4014)
            (NES-ppu:write-register (nes-ppu n) addr val)
            (NES-ppu:write-register (nes-ppu n) (mod addr 8) val))))

(defun console-on (n)
  (declare (nes n))
  (NES-ppu:reset-ppu (nes-ppu n))
  (setf (nes-cart n) (NES-cartridge:load-cartridge #P"/home/samanthadoran/nes/smb.nes"))
  (setf (NES-ppu:ppu-trigger-nmi-callback (nes-ppu n)) (6502-cpu:trigger-nmi-callback (nes-cpu n)))
  (setf (NES-ppu:ppu-oam-dma-callback (nes-ppu n)) (lambda (addr) (6502-cpu:read-cpu (nes-cpu n) addr)))
  (setf (NES-ppu:ppu-oam-stall-adder (nes-ppu n)) (6502-cpu:add-to-stall (nes-cpu n)))
  (setf
   (aref (the (simple-array function 1) (NES-ppu:ppu-memory-get (nes-ppu n))) 0)
   (ppu-to-mapper-read n))
  (setf
   (aref (the (simple-array function 1) (NES-ppu:ppu-memory-set (nes-ppu n))) 0)
   (ppu-to-mapper-write n))
  (setf
   (aref (the (simple-array function 1) (NES-ppu:ppu-memory-get (nes-ppu n))) 1)
   (ppu-to-name-table-read n))
  (setf
   (aref (the (simple-array function 1) (NES-ppu:ppu-memory-set (nes-ppu n))) 1)
   (ppu-to-name-table-write n))
  (setf
   (aref (the (simple-array function 1) (NES-ppu:ppu-memory-get (nes-ppu n))) 2)
   (ppu-to-palette-read n))
  (setf
   (aref (the (simple-array function 1) (NES-ppu:ppu-memory-set (nes-ppu n))) 2)
   (ppu-to-palette-write n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-get (nes-cpu n))) 0)
   (cpu-to-cpu-read n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-set (nes-cpu n))) 0)
   (cpu-to-cpu-write n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-get (nes-cpu n))) 1)
   (cpu-to-ppu-read n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-set (nes-cpu n))) 1)
   (cpu-to-ppu-write n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-get (nes-cpu n))) 5)
   (cpu-to-cart-read n))
  (6502-cpu:power-on (nes-cpu n)))

(defun step-nes (n steps)
  (declare (nes n))
  (declare ((unsigned-byte 32) steps))
  (loop for s from 1 to steps
    do
    (let ((cycles (* 3 (the (unsigned-byte 8)(6502-CPU:step-cpu (nes-cpu n))))))
      (declare ((unsigned-byte 8) cycles))
      (loop for i from 1 to cycles
        do
        (NES-ppu:step-ppu (nes-ppu n))))))

(defun step-frame (n)
  (declare (nes n))
  (let ((frame (logand #xFFFF (NES-ppu:ppu-frame (nes-ppu n)))))
    (declare ((unsigned-byte 16) frame))
    (loop
      do
      (progn
       (when (not (= frame (the (unsigned-byte 16)(logand #xFFFF (NES-ppu:ppu-frame (nes-ppu n)))))) (return))
       (step-nes n 1)))))

(defun test-render-clear (renderer)
  (progn (sdl2:set-render-draw-color renderer 0 0 0 255)
       (sdl2:render-clear renderer)))

(defun render-nes (front renderer)
  (loop for y from 0 to 239
    do
    (loop for x from 0 to 255
      do
      (sdl2:with-points ((p x y))
        (let* ((color (aref front y x))
               (r (color-r color))
               (g (color-g color))
               (b (color-b color)))
       (sdl2:set-render-draw-color renderer r g b 255)
       (multiple-value-bind (points num)
        (sdl2:points* p)
        (sdl2:render-draw-points renderer points num)))))))

(defun setup-and-emulate ()
  (let ((a (make-nes)))
    (console-on a)
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "SDL2 Renderer API Demo" :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (step-frame a)
             (test-render-clear renderer)
             (render-nes (NES-ppu:ppu-front (nes-ppu a)) renderer)
             (sdl2:render-present renderer))
            (:quit () t)))))))
