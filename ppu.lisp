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

(defstruct ppu
  "A model picture processing unit"
  (control 0 :type (unsiged-byte 8))
  (mask 0 :type (unsigned-byte 8))
  (status 0 :type (unsigned-byte 8))
  (oamaddr 0 :type (unsigned-byte 8))
  (scroll 0 :type (unsigned-byte 8))
  (address 0 :type (unsigned-byte 8))
  (data 0 :type (unsigned-byte 8))
  (oam-address 0 :type (unsigned-byte 16))

  (scanline 0)
  (scroll-x 0)
  (scroll-y 0)
  (cycle 0)

  (oddframe nil)
  (screen (make-array (* screen-width screen-height) :element-type '(unsigned-byte 8)))
  (vram 0))

;PPU Control helper functions
(defun wrap-byte (val)
  (logand #xFF val))

(defun wrap-word (val)
  (logand #xFFFF val))

(defun x-scroll-offset (p)
  (if (= (logand (ppu-control p) #x01) 0)
    0
    256))

(defun y-scroll-offset (p)
  (if (= (logand (ppu-control p) #x02) 0)
    0
    240))

(defun vram-address-increment (p)
  (if (= (logand (ppu-control p) #x04) 0)
    1
    32))

(defun sprite-pattern-table-address (p)
  (if (= (logand (ppu-control p) #x08) 0)
    0
    #x1000))

(defun background-pattern-table-address (p)
  (if (= (logand (ppu-control p) #x10) 0)
    0
    #x1000))

(defun sprite-size (p)
  (if (= (logand (ppu-control p) #x20) 0)
    :sprite-size-8-by-8
    :sprite-size-8-by-16))

(defun is-blank-nmi (p)
  (not (= (logand (ppu-control p) #x80) 0)))

;PPU Mask helper functions
(defun show-background (p)
  (not (= (logand (ppu-mask p) #x08) 0)))

(defun show-sprites (p)
  (not (= (logand (ppu-mask p) #x10) 0)))

;PPU Status helper functions
(defun set-sprite-overflow (p val)
  (setf
   (ppu-status p)
   (if val
     (logior (ppu-status p) #x20)
     (logand (ppu-status p) (lognot #x20)))))

(defun set-sprite-zero-hit (p val)
  (setf
   (ppu-status p)
   (if val
     (logior (ppu-status p) #x40)
     (logand (ppu-status p) (lognot #x40)))))

(defun set-vblank-nmi (p val)
  (setf
   (ppu-status p)
   (if val
     (logior (ppu-status p) #x80)
     (logand (ppu-status p) (lognot #x80)))))

(defun power-on (p)
  (setf (ppu-control p) 0)
  (setf (ppu-mask p) 0)
  (setf (ppu-oam-address p) 0)
  (setf (ppu-scroll-x p) 0)
  (setf (ppu-scroll-y p) 0)
  (setf (ppu-address p) 0)
  (setf (ppu-data p) 0)
  (setf (ppu-odd-frame p) nil))

(defun reset (p)
  (setf (ppu-control p) 0)
  (setf (ppu-mask p) 0)
  (setf (ppu-scroll-x p) 0)
  (setf (ppu-scroll-y p) 0)
  (setf (ppu-data p) 0)
  (setf (ppu-odd-frame p) nil))
