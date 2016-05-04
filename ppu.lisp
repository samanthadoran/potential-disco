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
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8)))

(defstruct ppu
  "A model picture processing unit"
  (front (make-array '(240 256) :element-type 'color :initial-element (make-color :r 0 :g 0 :b 0)))
  (back (make-array '(240 256) :element-type 'color :initial-element (make-color :r 0 :g 0 :b 0)))

  (nmi-callback 0)

  (cycle 0)
  (scanline 0)
  (frame 0)

  (palette-data (make-array 32 :element-type '(unsigned-byte 8)))
  (name-table-data (make-array 2048 :element-type '(unsigned-byte 8)))
  (oam-data (make-array 256 :element-type '(unsigned-byte 8)))

  ;Registers
  (v 0 :type (unsigned-byte 15)) ;Current vram address
  (tv 0 :type (unsigned-byte 15));Temporary vram address
  (x 0 :type (unsigned-byte 3));Fine x scroll
  (w 0 :type (unsigned-byte 1));Write toggle
  (f 0 :type (unsigned-byte 1));Odd frame flag
  (register 0 :type (unsigned-byte 8))

  ;NMI Status
  (nmi-occurred nil)
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
  (flag-increment 0 :type (unsigned-byte 1))
  (flag-sprite-table 0 :type (unsigned-byte 1))
  (flag-background-table 0 :type (unsigned-byte 1))
  (flag-sprite-size 0 :type (unsigned-byte 1))
  (flag-master-slave 0 :type (unsigned-byte 1))

  ;$2001 PPU Mask
  (flag-grayscale 0 :type (unsigned-byte 1))
  (flag-show-left-background 0 :type (unsigned-byte 1))
  (flag-show-left-sprites 0 :type (unsigned-byte 1))
  (flag-show-background 0 :type (unsigned-byte 1))
  (flag-show-sprites 0 :type (unsigned-byte 1))
  (flag-red-tint 0 :type (unsigned-byte 1))
  (flag-green-tint 0 :type (unsigned-byte 1))
  (flag-blue-tint 0 :type (unsigned-byte 1))

  ;$2002 PPU Status
  (flag-sprite-zero-hit 0 :type (unsigned-byte 8))
  (flag-sprite-overflow 0 :type (unsigned-byte 8))

  ;$2003 OAM Address
  (oam-address 0 :type (unsigned-byte 8))

  ;Buffer for $2007 Data Read
  (buffered-data 0 :type (unsigned-byte 8)))

(defun wrap-byte (val)
  (logand #xFF val))

(defun wrap-word (val)
  (logand #xFFFF val))

(defun nmi-change (p)
  (let ((nmi (and (ppu-nmi-output p) (ppu-nmi-occurred p))))
    (when (and nmi (ppu-nmi-previous p))
      (setf (ppu-nmi-delay p) 15))
    (setf (ppu-nmi-previous p) nmi)))

(defun read-palette (p address)
  (aref
   (ppu-palette-data p)
   (if (and (>= address 16) (= (mod address 4) 0))
     (wrap-word (- address 16))
     address)))

(defun write-palette (p address value)
  (setf
   (aref
    (ppu-palette-data p)
    (if (and (>= address 16) (= (mod address 4) 0))
      (wrap-word (- address 16))
      address))
   value))

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
   (ppu-tv p)
   ;Keep it in 15 bits
   (logand
    #x7FFF
    (logior
     (logand (ppu-tv p) #xF3FF)
     (ash (logand value 3) -10)))))

(defun write-mask (p value)
  (setf
   (ppu-flag-grayscale p)
   (logand (ash value 0) 1))
  (setf
   (ppu-flag-show-left-background p)
   (logand (ash value -1) 1))
  (setf
   (ppu-flag-show-left-sprites p)
   (logand (ash value -2) 1))
  (setf
   (ppu-flag-show-background p)
   (logand (ash value -3) 1))
  (setf
   (ppu-flag-show-sprites p)
   (logand (ash value -4) 1))
  (setf
   (ppu-flag-red-tint p)
   (logand (ash value -5) 1))
  (setf
   (ppu-flag-green-tint p)
   (logand (ash value -6) 1))
  (setf
   (ppu-flag-blue-tint p)
   (logand (ash value -7) 1)))

(defun read-status (p)
  (setf
   (ppu-w p)
   0)
  (let ((result (logand (ppu-register p) #x1F)))
    (setf
     result
     (logior
      (logior
       result
       (wrap-byte (ash (ppu-flag-sprite-overflow p) -5)))
      (wrap-byte (ash (ppu-flag-sprite-zero-hit p) -6))))
    (setf
     result
     (if (ppu-nmi-occurred p)
       (logior
        result
        (wrap-byte (ash 1 -7)))
       result))
    (setf
     (ppu-nmi-occurred p)
     nil)
    (nmi-change p)
    result))


(defun write-oam-address (p value)
  (setf
   (ppu-oam-address p)
   value))

(defun read-oam-data (p)
  (aref
   (ppu-oam-data p)
   (ppu-oam-address p)))

(defun write-oam-data (p value)
  (setf
   (aref
    (ppu-oam-data p)
    (ppu-oam-address p))
   value)
  (setf
   (ppu-oam-address p)
   (wrap-byte (1+ (ppu-oam-address p)))))

(defun write-scroll (p value)
  (if (= (ppu-w p) 0)
    (progn
     (setf
      (ppu-tv p)
      (logior
       (logand
        (ppu-tv p)
        #xFFE0)
       (ash value -3)))
     (setf
      (ppu-x p)
      (logand value #x07))
     (setf
      (ppu-w p)
      1))
    (progn
     (setf
      (ppu-tv p)
      (logior
       (logand
        (ppu-tv p)
        #x8FFF)
       (ash (logand value #x07) 12)))
     (setf
      (ppu-tv p)
      (logior
       (logand (ppu-tv p) #xFC1F)
       (ash (logand value #xF8) 2)))
     (setf
      (ppu-w p)
      0))))

(defun write-address (p value)
  (if (= (ppu-w p) 0)
    (progn
     (setf
      (ppu-tv p)
      (logior
       (logand (ppu-tv p) #x80FF)
       (ash (logand value #x3F) 8)))
     (setf (ppu-w p) 1))
    (progn
     (setf
      (ppu-tv p)
      (logior
       (logand (ppu-tv p) #xFF00)
       value))
     (setf (ppu-tv p) (ppu-tv p))
     (setf (ppu-w p) 0))))


(defun read-data (p)
  (let ((value (read-ppu p (ppu-v p))))
    (if (< (mod (ppu-v p) #x4000) #x3F00)
      (progn
       (let ((buffered (ppu-buffered-data p)))
         (setf
          (ppu-buffered-data p)
          value)
         (setf
          value
          buffered)))
      (setf
       (ppu-buffered-data p)
       (read-ppu p (- (ppu-v p) #x1000))))
    (setf
     (ppu-v p)
     (wrap-word
      (+
       (ppu-v p)
       (if (= (ppu-flag-increment p) 0)
         1
         32))))
    value))


(defun write-data (p value)
  (write-ppu p (ppu-v p) value)
  (setf
   (ppu-v p)
   (wrap-word
    (+
     (ppu-v p)
     (if (= (ppu-flag-increment p) 0)
       1
       32)))))

;TODO: Implement
(defun write-dma (p value)
  (declare (ignore p value)))
; // $4014: OAMDMA
; func (ppu *PPU) writeDMA(value byte) {
; 	cpu := ppu.console.CPU
; 	address := uint16(value) << 8
; 	for i := 0; i < 256; i++ {
; 		ppu.oamData[ppu.oamAddress] = cpu.Read(address)
; 		ppu.oamAddress++
; 		address++
; 	}
; 	cpu.stall += 513
; 	if cpu.Cycles%2 == 1 {
; 		cpu.stall++
; 	}
; }

(defun read-register (p selector)
  (cond
    ;Read ppu status
    ((= selector 2) (read-status p))
    ;Read OAM Data
    ((= selector 4) (read-oam-data p))
    ;Read Data
    ((= selector 7) (read-data p))
    ;Default case
    (T 0)))

(defun write-register (p selector value)
  (setf
   (ppu-register p)
   value)
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
    (T (format t "We really can't write to register 0x~x" selector))))

(defun reset (p)
  (setf
   (ppu-cycle p)
   340)
  (setf
   (ppu-scanline p)
   240)
  (setf
   (ppu-frame p)
   0)
  (write-control p 0)
  (write-mask p 0)
  (write-oam-address p 0))
