(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "sdl2"))

(defpackage #:NES-console
  (:nicknames #:nes)
  (:use :cl :cl-user :6502-cpu :NES-cartridge :NES-ppu :NES-controller)
  (:export #:make-nes #:console-on #:nes-cpu #:nes-ppu #:nes-cart #:step-nes
           #:step-frame #:setup-and-emulate #:render-nes #:read-rom))

(in-package :NES-console)
(declaim (optimize (speed 3) (safety 1)))
(defstruct nes
  "A model nes"
  (cpu (6502-cpu:make-cpu))
  (cart (NES-cartridge:make-cartridge))
  (ppu (NES-ppu:make-ppu))
  (controllers
   (make-array 2 :initial-contents `(,(nes-controller:make-controller)
                                     ,(nes-controller:make-controller)))))

(defvar mirror-lookup
  (make-array
   20
   :element-type '(unsigned-byte 2)
   :initial-contents '(0 0 1 1 0 1 0 1 0 0 0 0 1 1 1 1 0 1 2 3)))

(defun mirror-address (mode addr)
  (declare ((unsigned-byte 16) addr))
  (declare ((unsigned-byte 8) mode))
  (let* ((address (mod (- addr #x2000) #x1000))
        (table (floor address #x0400))
        (offset (mod address #x0400)))
    (declare ((unsigned-byte 16) address table offset))
    (logand #xFFFF (+ #x2000 offset (* #x0400 (the (unsigned-byte 3) (aref (the (simple-array (unsigned-byte 2) 1)mirror-lookup) (+ (* mode 4) table))))))))

(defun ppu-to-name-table-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (aref
           (the (simple-array (unsigned-byte 8) 1) (NES-ppu:ppu-name-table-data (nes-ppu n)))
           (logand (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) #x7ff))))

(defun ppu-to-name-table-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1) (NES-ppu:ppu-name-table-data (nes-ppu n)))
            (logand (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) #x7ff))
           val)))

(defun ppu-to-palette-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (NES-ppu:read-palette (nes-ppu n) (logand addr #x1f))))

(defun ppu-to-palette-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (NES-ppu:write-palette (nes-ppu n) (logand addr #x1f) val)))

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
           (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-prg-rom (nes-cart n)))
           (mod
            addr
            (array-dimension (the (simple-array (unsigned-byte 8) 1)(NES-cartridge:cartridge-prg-rom (nes-cart n))) 0)))))

(defun cpu-to-cart-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr))
          (declare ((unsigned-byte 8) val))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1)(NES-cartridge:cartridge-prg-rom (nes-cart n)))
            (mod
             addr
             (array-dimension (the (simple-array (unsigned-byte 8) 1)(NES-cartridge:cartridge-prg-rom (nes-cart n))) 0)))
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

(defun cpu-to-io-read (n)
  (declare (nes n))
  (lambda (addr)
          (declare ((unsigned-byte 16) addr))
          (if (or (= addr #x4016) (= addr #x4017))
            (NES-controller:read-controller
             (aref
              (the (simple-array nes-controller:controller 1)(nes-controllers n))
              (mod addr 2)))
            0)))

(defun cpu-to-io-write (n)
  (declare (nes n))
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val))
          (if (or (= addr #x4016) (= addr #x4017))
            (NES-controller:write-controller
             (aref
              (the (simple-array nes-controller:controller 1)(nes-controllers n))
              (mod addr 2))
             val)
            0)))

(defun read-rom (n rom-name)
  (declare (nes n))
  (setf (nes-cart n) (NES-cartridge:load-cartridge rom-name)))

(defun get-buttons()
  (let ((buttons (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref buttons 0) (if (sdl2:keyboard-state-p :scancode-left) 1 0))
    (setf (aref buttons 1) (if (sdl2:keyboard-state-p :scancode-down) 1 0))
    (setf (aref buttons 2) (if (sdl2:keyboard-state-p :scancode-grave) 1 0))
    (setf (aref buttons 3) (if (sdl2:keyboard-state-p :scancode-tab) 1 0))
    (setf (aref buttons 4) (if (sdl2:keyboard-state-p :scancode-w) 1 0))
    (setf (aref buttons 5) (if (sdl2:keyboard-state-p :scancode-s) 1 0))
    (setf (aref buttons 6) (if (sdl2:keyboard-state-p :scancode-a) 1 0))
    (setf (aref buttons 7) (if (sdl2:keyboard-state-p :scancode-d) 1 0))
    buttons))

(defun console-on (n)
  (declare (nes n))
  (NES-ppu:reset-ppu (nes-ppu n))
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
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-get (nes-cpu n))) 2)
   (cpu-to-io-read n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-set (nes-cpu n))) 2)
   (cpu-to-io-write n))
  (setf
   (aref (the (simple-array function 1) (6502-cpu:cpu-memory-get (nes-cpu n))) 5)
   (cpu-to-cart-read n))
  (6502-cpu:power-on (nes-cpu n))
  (setf
   (NES-controller:controller-buttons-callback (aref (the (simple-array NES-controller:controller 1)(nes-controllers n)) 0))
   #'get-buttons))

(defun step-nes (n steps)
  (declare (nes n))
  (declare ((unsigned-byte 32) steps))
  (loop for s from 1 to steps
    do
    (let ((cycles (* 3 (the (unsigned-byte 8)(6502-CPU:step-cpu (nes-cpu n))))))
      (declare ((unsigned-byte 8) cycles))
      (NES-ppu:step-ppu (nes-ppu n) cycles))))

(defun step-frame (n)
  (declare (nes n))
  (let ((frame (NES-ppu:ppu-frame (nes-ppu n)))
        (controller (aref (the (simple-array nes-controller:controller 1)(nes-controllers n)) 0)))
    (declare ((unsigned-byte 16) frame))
    (loop
      do
      (progn
       (when (not (= frame (NES-ppu:ppu-frame (nes-ppu n)))) (return))
       (nes-controller:update-controller controller)
       (step-nes n 1)))))

(defun test-render-clear (renderer)
  (progn (sdl2:set-render-draw-color renderer 0 0 0 255)
       (sdl2:render-clear renderer)))

(defun render-nes (front renderer tex rect)
  (multiple-value-bind
   (pixels pitch)
   (sdl2:lock-texture tex rect)
   ;(print pitch)
   (loop for y from 0 to (- NES-ppu:screen-height 1)
    do
    (loop for x from 0 to (- NES-ppu:screen-width 1)
      do
      (let* ((color (aref (the (simple-array NES-ppu:color 1) front) (+ (* y NES-ppu:screen-width) x)))
             (r (color-r color))
             (g (color-g color))
             (b (color-b color))
             (col (logior (ash #xFF 24) (ash r 16) (ash g 8) (ash b 0))))
        (setf (sb-sys:sap-ref-32 pixels (* 4 (+ (* y NES-ppu:screen-width) x))) col))))
   (sdl2:update-texture tex pixels :rect rect :width pitch)
   (sdl2:unlock-texture tex))
  (sdl2:render-copy renderer tex :dest-rect rect))

(defun setup-and-emulate (cart-name)
  (let ((a (make-nes)))
    (read-rom a cart-name)
    (console-on a)
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Potential-Disco" :w NES-ppu:screen-width :h NES-ppu:screen-height :flags '(:shown))
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (let* ((tex (sdl2:create-texture
                      renderer
                      :argb8888
                      :streaming
                      NES-ppu:screen-width
                      NES-ppu:screen-height))
                (rect (sdl2:make-rect 0 0 NES-ppu:screen-width NES-ppu:screen-height)))
            (sdl2:with-event-loop (:method :poll)
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             ;Update Controller
             (step-frame a)
             (test-render-clear renderer)
             (render-nes (NES-ppu:ppu-front (nes-ppu a)) renderer tex rect)
             (sdl2:render-present renderer))
            (:quit () t))))))))
