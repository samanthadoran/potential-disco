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

(defun render-pixel (p)
  )

(defun fetch-sprite-pattern (p i r)
  (let (
        (tile (aref (ppu-oam-data p) (+ (* i 4) 1)))
        (attributes (aref (ppu-oam-data p) (+ (* i 4) 2)))
        (address 0))
    (if (= (ppu-flag-sprite-size p) 0)
      (progn
       (when (= (logand attributes #x80) #x80)
         (setf row (- 7 row)))
       (setf table (ppu-flag-sprite-table p))
       (setf address (+(* #x1000 (wrap-word table)) (wrap-word tile) (int-to-uint16 row))))
      (progn
       (when (= (logand attributes #x80) #x80)
         (setf row (- 15 row)))
       (setf table (logand table 1))
       (setf tile (logand tile #xFE))
       (when (> row 7)
         (incf tile)
         (decf row 8))
       (setf address (+(* #x1000 (wrap-word table)) (wrap-word tile) (int-to-uint16 row)))))
    (let (
          (a (ash (logand attributes 3) 2))
          (low-tile-byte (read-ppu address))
          (high-tile-byte (wrap-word(read-ppu address + 8)))
          (data 0))
      (loop for i from 0 to 7
        do
        (let ((p1 0) (p2 0))
          (if (= (logand attributes #x40) #x40)
            (progn
             (setf p1 (ash (logand low-tile-byte 1) 0))
             (setf p2 (ash (logand high-tile-byte 1) 1))
             (setf high-tile-byte (ash high-tile-byte -1))
             (setf low-tile-byte (ash low-tile-byte -1)))
            (progn
              (setf p1 (ash (logand low-tile-byte #x80) 7))
              (setf p2 (ash (logand high-tile-byte #x80) 6))
              (setf high-tile-byte (ash high-tile-byte 1))
              (setf low-tile-byte (ash low-tile-byte 1))))
          (setf data (ash data 4))
          ;Make sure to make it an (unsigned-byte 32)
          (setf data (logand #xFFFFFFFF (logior data a p1 p2)))))
      data)))

(defun evaluate-sprites (p)
  (let ((count 0)(h (if (ppu-flag-sprite-size p) 16 8)))
    (loop for i from 0 to 63
      do
      (
       (let (
             (y (aref (ppu-oam-data p) (+ (* i 4))))
             (a (aref (ppu-oam-data p) (+ (* i 4) 2)))
             (x (aref (ppu-oam-data p) (+ (* i 4) 3)))
             (row (- (ppu-scanline p) y)))
         (when (not (and (>= row 0) (<= row h)))
           (when (< count 8)
             (setf (aref (ppu-sprite-patterns p) count) (fetch-sprite-pattern i row))
             (setf (aref (ppu-sprite-positions p) count) x)
             (setf (aref (ppu-sprite-priorities p) count) (logand (ash a -5) 1))
             (setf (aref (ppu-sprite-indices p) count) (logand #xFF i)))
           (incf count)))))
    (when (> count 8)
      (setf count 8)
      (setf (ppu-flag-sprite-overflow) 1))
    (setf (ppu-sprite-count p) count)))

(defun tick (p)
  (when (> 0 (ppu-nmi-delay p))
    (decf (ppu-nmi-delay p))
    (when (and (= (ppu-nmi-delay p) 0) (ppu-nmi-output p) (ppu-nmi-occured p))
      (funcall (ppu-nmi-callback p))))

  (when (or (not (= 0 (ppu-flag-show-background p))) (not (= 0(ppu-flag-show-sprites p))))
    (when (and (= (ppu-f) 1) (= (ppu-scanline p) 261) (= (ppu-cycle p) 339))
      (setf (ppu-cycle p) 0)
      (setf (ppu-scanline) 0)
      (incf (ppu-frame p))
      (setf (ppu-f p) (logxor (ppu-f p xor 1)))
      (return-from tick nil)))

  (incf (ppu-cycle p))
  (when (> (ppu-cycle p) 340)
    (setf (ppu-cycle p) 0)
    (incf (ppu-scanline p))
    (when (> (ppu-scanline) 261)
      (setf (ppu-scanline p) 0)
      (incf (ppu-frame p))
      (setf (ppu-f p) (logxor (ppu-f p) 1)))))

(defun step (p)
  (tick p)
  (let* (
         (rendering-enabled (not (= (ppu-flag-show-backgroundp) 0)))
         (pre-line (= (ppu-scanline p) 261))
         (visible-line (< (ppu-scanline p) 240))
         (render-line (or pre-line visible-line))
         (pre-fetch-cycle (and (>= (ppu-cycle p) 321) (<= (ppu-cycle p) 336)))
         (visible-cycle (and (>= (ppu-cycle p) 1) (<= (ppu-cycle p) 256)))
         (fetch-cycle (or pre-fetch-cycle visible-cycle)))
    ;Background logic
    (when rendering-enabled
      (when (and visible-line visible-cycle)
        (render-pixel p))
      (when (and render-line fetch-cycle)
        (setf
         (ppu-tile-data p)
         (logand ;Make sure it its in 64 bits
          (ash
           (ppu-tile-data p)
           4)
          #xFFFFFFFFFFFFFFFF))
        (let ((c (mod (ppu-cycle p) 8)))
          (cond
            ((= c 0) (store-tile-data p))
            ((= c 1) (fetch-name-table-byte p))
            ((= c 3) (fetch-low-tile-byte p))
            ((= c 5) (fetch-high-tile-byte p)))))
      (when (and preline (>= (ppu-cycle p) 280) (<= (ppu-cycle p) 304))
        (copy-y p))
      (when render-line
        (when (and fetch-cycle (= (mod (ppu-cycle p) 8) 0))
          (increment-x p))
        (when (= (ppu-cycle p) 256)
          (increment-y p))
        (when (= (ppu-cycle p) 257)
          (copy-x p))))
    ;Sprite logic
    (when rendering-enabled
      (if (= (ppu-cycle) 257)
        (evaluate-sprites p)
        (setf (ppu-sprite-count p) 0)))
    ;Vblank logic
    (when (and (= (ppu-scanline p) 241) (= (ppu-cycle p) 1))
      (set-vertical-blank p))
    (when (and preline (= (ppu-cycle p) 1))
      "(clear-vertical-blank p)"
      (setf (ppu-flag-sprite-zero-hit) 0)
      (setf (ppu-flag-sprite-overflow) 0))))
