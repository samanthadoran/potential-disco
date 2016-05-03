;; Almost all of my understanding of the PPU comes from..
;;  The nesdev wiki
;;  Sprocket-NES
;;  Fogleman's NES
;;  Famiclom

(in-package :cl-user)

(defpackage #:NES-ppu
  (:nicknames #:ppu)
  (:use :cl :cl-user)
  (:export ))

(in-package :NES-ppu)

(defconstant screen-width 256)
(defconstant screen-height 240)
(defconstant vblank-scanline 241)
(defconstant last-scanline 261)
(defconstant cycles-per-scanline 114)
(defconstant cycles-per-cpu 3)

(defstruct color
  "Simple RGBA"
  (r 0 :type (unsiged-byte 8))
  (g 0 :type (unsiged-byte 8))
  (b 0 :type (unsiged-byte 8)))

(defstruct ppu
  "A model picture processing unit"
  (front (make-array '(240 256) :element-type 'color))
  (back (make-array '(240 256) :element-type 'color))

  (nmi-callback 0)

  (cycle 0)
  (scanline 0)
  (frame 0)

  (palette-data (make-array 32 :element-type '(unsigned-byte 8)))
  (name-table-data (make-array 2048 :element-type '(unsigned-byte 8)))
  (oam-data (make-array 256 :element-type '(unsigned-byte 8)))
  (front 0)
  (back 0)

  ;Registers
  (v 0 :type (unsigned-byte 15)) ;Current vram address
  (tv 0 :type (unsigned-byte 15));Temporary vram address
  (x 0 :type (unsigned-byte 3));Fine x scroll
  (w 0 :type (unsigned-byte 1));Write toggle
  (f 0 :type (unsigned-byte 1));Odd frame flag
  (register 0 :type (unsigned-byte 8))

  ;NMI Status
  (nmi-occured nil)
  (nmi-output nil)
  (nmi-previous nil)
  (nmi-delay 0)

  ;Tiles
  (name-table 0 :type (unsigned-byte 8))
  (attribute-table 0 :type (unsigned-byte 8))
  (low-tile 0 :type (unsigned-byte 8))
  (high-tile 0 :type (unsigned-byte 8))
  (tile-data 0 :type (unsigned-byte 64))

  ;Sprites
  (sprite-count 0)
  (sprite-patterns (make-array 8 :element-type '(unsigned-byte 32)))
  (sprite-positions (make-array 8 :element-type '(unsigned-byte 8)))
  (sprite-priorities (make-array 8 :element-type '(unsigned-byte 8)))
  (sprite-indices (make-array 8 :element-type '(unsigned-byte 8)))

  ;$2000 PPU Control
  (flag-name-table 0 :type (unsigned-byte 2))
  (flag-increment nil)
  (flag-sprite-table nil)
  (flag-background-table nil)

  ;$2001 PPU Mask
  (flag-grayscale nil)
  (flag-show-left-background nil)
  (flag-show-left-sprites nil)
  (flag-show-background nil)
  (flag-show-sprites nil)
  (flag-red-tint nil)
  (flag-green-tint nil)
  (flag-blue-tint nil)

  ;$2002 PPU Status
  (flag-sprite-zero-hit nil)
  (flag-sprite-overflow nil)

  ;$2003 OAM Address
  (oam-address 0 :type '(unsigned-byte 8))

  ;Buffer for $2007 Data Read
  (buffered-data 0 :type '(unsigned-byte 8)))

;TODO: Implement
(defun read-palette (p address)
  (declare (ignore p address))
  0)

;TODO: Implement
(defun write-palette (p address value)
  (declare (ignore p address value))
  0)

(defun read-register (p selector)
  (cond
    ;Read ppu status
    ((= selector 2) 0)
    ;Read OAM Data
    ((= selector 4) 0)
    ;Read Data
    ((= selector 7) 0)
    ;Default case
    (T 0)))

(defun write-control (p value)
  (setf
   (ppu-flag-name-table p)
   (logand (ash value 0) 3))
  (setf
   (ppu-flag-increment p)
   (logand (ash value -2) 1))
  (setf
   (ppu-flag-sprite-table p)
   (logand (ash value -3) 1))
  (setf
   (ppu-flag-background-table p)
   (logand (ash value -4) 1))
  (setf
   (ppu-flag-sprite-size p)
   (logand (ash value -5) 1))
  (setf
   (ppu-flag-master-slave p)
   (logand (ash value -6) 1))
  (setf
   (ppu-nmi-output p)
   (= (logand (ash value -7) 1) 1))
  (nmi-change p)
  (setf
   (ppu-t p)
   ;Keep it in 15 bits
   (logand
    #x7FFF
    (logior
     (logand (ppu-t p) #xF3FF)
     (ash (logand value 3) -10)))))

(defun write-register (p selector value)
  (setf
   (ppu-register p)
   (value))
  (cond
    ;Write Control
    ((= selector 0) (write-control p value))
    ;Write Mask
    ((= selector 1) (write-mask p value))
    ;Write OAM Address
    ((= selector 3) (write-oam-address p value))
    ;Write OAM Data
    ((= selector 4) (write-oam-data p value))
    ;Write scroll
    ((= selector 5) (write-scroll p value))
    ;Write Address
    ((= selector 6) (write-address p value))
    ;Write Data
    ((= selector 7) (write-data p value))
    ;Write DMA
    ((= selector #x14) (write-dma p value))
    ((T (format t "We really can't write to register 0x~x" selector)))))
