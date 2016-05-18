;; Almost all of my understanding of the PPU comes from..
;;  The nesdev wiki
;;  Sprocket-NES
;;  Fogleman's NES
;;  Famiclom

(in-package :cl-user)

(defpackage #:NES-ppu
  (:nicknames #:ppu)
  (:use :cl :cl-user)
  (:export #:step-ppu #:make-ppu #:reset-ppu #:read-register #:write-register
           #:ppu-trigger-nmi-callback #:ppu-front #:ppu-back #:ppu-frame
           #:color-r #:color-g #:color-b #:color #:read-palette #:write-palette
           #:ppu-memory-get #:ppu-memory-set #:ppu-name-table-data
           #:ppu-oam-dma-callback #:ppu-oam-stall-adder #:screen-width #:screen-height))

(in-package :NES-ppu)
(declaim (optimize (speed 3) (safety 1)))
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

(defun wrap-byte (val)
  (the (unsigned-byte 8) (logand #xFF val)))

(defun wrap-word (val)
  (the (unsigned-byte 16) (logand #xFFFF val)))

(defun to-signed-byte-8 (val)
  (declare ((unsigned-byte 8) val))
  (the fixnum (if (ldb-test (byte 1 7) val)
    (* -1 (wrap-byte (1+ (lognot val))))
    val)))

(defvar
  *palette*
  (progn
   (let ((pal (make-array 64 :element-type 'color :initial-element (make-color :r 0 :g 0 :b 0)))
         (colors
          (make-array
           64
           :element-type '(unsigned-byte 32)
           :initial-contents
           '(#x666666 #x002A88 #x1412A7 #x3B00A4 #x5C007E #x6E0040 #x6C0600 #x561D00
	           #x333500 #x0B4800 #x005200 #x004F08 #x00404D #x000000 #x000000 #x000000
	           #xADADAD #x155FD9 #x4240FF #x7527FE #xA01ACC #xB71E7B #xB53120 #x994E00
	           #x6B6D00 #x388700 #x0C9300 #x008F32 #x007C8D #x000000 #x000000 #x000000
	           #xFFFEFF #x64B0FF #x9290FF #xC676FF #xF36AFF #xFE6ECC #xFE8170 #xEA9E22
             #xBCBE00 #x88D800 #x5CE430 #x45E082 #x48CDDE #x4F4F4F #x000000 #x000000
	           #xFFFEFF #xC0DFFF #xD3D2FF #xE8C8FF #xFBC2FF #xFEC4EA #xFECCC5 #xF7D8A5
	           #xE4E594 #xCFEF96 #xBDF4AB #xB3F3CC #xB5EBF2 #xB8B8B8 #x000000 #x000000))))
     (loop for i from 0 to 63
       do
       (let* ((c (aref colors i))
             (r (wrap-byte (ash c -16)))
             (g (wrap-byte (ash c -8)))
             (b (wrap-byte c)))
         (setf (aref pal i) (make-color :r r :g g :b b))))
     pal)))



(defstruct ppu
  "A model picture processing unit"
  (front
   (make-array #xF000 :element-type 'color :initial-element (make-color :r 0 :g 0 :b 0))
   :type (simple-array color 1))
  (back
   (make-array #xF000 :element-type 'color :initial-element (make-color :r 0 :g 0 :b 0))
   :type (simple-array color 1))

  (cycle 0 :type (unsigned-byte 16))
  (scanline 0 :type (unsigned-byte 16))
  (frame 0 :type (unsigned-byte 16))

  (memory-get
   (make-array 3 :element-type 'function :initial-element (lambda ()))
   :type (simple-array function 1))
  (memory-set
   (make-array 3 :element-type 'function :initial-element (lambda ()))
   :type (simple-array function 1))

  (palette-data
   (make-array 32 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))
  (name-table-data
   (make-array 2048 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))
  (oam-data
   (make-array 256 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))

  ;Registers
  (v 0 :type (unsigned-byte 16)) ;Current vram address
  (tv 0 :type (unsigned-byte 16));Temporary vram address
  (x 0 :type (unsigned-byte 3));Fine x scroll
  (w 0 :type (unsigned-byte 1));Write toggle
  (f 0 :type (unsigned-byte 1));Odd frame flag
  (register 0 :type (unsigned-byte 8))

  ;NMI Status
  (nmi-occurred nil)
  (nmi-output nil)
  (nmi-previous nil)
  (nmi-delay 0 :type (unsigned-byte 16))

  (trigger-nmi-callback (lambda ()) :type function)

  ;Tiles
  (name-table 0 :type (unsigned-byte 8))
  (attribute-table 0 :type (unsigned-byte 8))
  (low-tile 0 :type (unsigned-byte 8))
  (high-tile 0 :type (unsigned-byte 8))
  (tile-data 0 :type (unsigned-byte 64))

  ;Sprites
  (sprite-count 0 :type (unsigned-byte 8))
  (sprite-patterns
   (make-array 8 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) 1))
  (sprite-positions
   (make-array 8 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))
  (sprite-priorities
   (make-array 8 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))
  (sprite-indexes
   (make-array 8 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) 1))

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
  (buffered-data 0 :type (unsigned-byte 8))
  (oam-dma-callback (lambda()) :type function)
  (oam-stall-adder (lambda()) :type function))

(defun read-ppu (p addr)
  (declare (ppu p))
  (declare ((unsigned-byte 16) addr))
  (setf addr (mod addr #x4000))
  (cond
    ;Mapper
    ((< addr #x2000) (funcall (aref (ppu-memory-get p) 0) addr))
    ;Name table data
    ((< addr #x3F00) (funcall (aref (ppu-memory-get p) 1) addr))
    ;Palette data
    ((< addr #x4000) (funcall (aref (ppu-memory-get p) 2) addr))))

(defun write-ppu (p addr val)
  (declare (ppu p))
  (declare ((unsigned-byte 16) addr))
  (declare ((unsigned-byte 8) val))
  (setf addr (mod addr #x4000))
  (cond
    ;Mapper
    ((< addr #x2000) (funcall (aref (ppu-memory-set p) 0) addr val))
    ;Name table data
    ((< addr #x3F00) (funcall (aref (ppu-memory-set p) 1) addr val))
    ;Palette data
    ((< addr #x4000) (funcall (aref (ppu-memory-set p) 2) addr val))))

(defun read-palette (p address)
  (declare (ppu p))
  (declare ((unsigned-byte 16) address))
  (aref
   (ppu-palette-data p)
   (if (and (>= address 16) (= (mod address 4) 0))
     (logand #xFFFF (- address 16))
     address)))

(defun write-palette (p address value)
  (declare (ppu p))
  (declare ((unsigned-byte 16) address))
  (declare ((unsigned-byte 8) value))
  (setf
   (aref
    (ppu-palette-data p)
    (if (and (>= address 16) (= (mod address 4) 0))
      (logand #xFFFF (- address 16))
      address))
   value))

(defun write-control (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
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
   (wrap-word
    (logior
     (logand (ppu-tv p) #xF3FF)
     (ash (logand value 3) 10)))))

(defun write-mask (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
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
  (setf (ppu-w p) 0)
  (let ((result (logand (ppu-register p) #x1F)))
    (declare ((unsigned-byte 8) result))
    (setf
     result
     (logior
      (logior
       result
       (ash (ppu-flag-sprite-overflow p) 5))
      (ash (ppu-flag-sprite-zero-hit p) 6)))
    (when (ppu-nmi-occurred p)
      (setf result (logior result (ash 1 7))))
    (setf (ppu-nmi-occurred p) nil)
    (nmi-change p)
    (wrap-byte result)))


(defun write-oam-address (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
  (setf (ppu-oam-address p) value))

(defun read-oam-data (p)
  (declare (ppu p))
  (aref (ppu-oam-data p) (ppu-oam-address p)))

(defun write-oam-data (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
  (setf (aref (ppu-oam-data p) (ppu-oam-address p)) value)
  (setf (ppu-oam-address p) (wrap-byte (1+ (ppu-oam-address p)))))

(defun write-scroll (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
  (if (= (ppu-w p) 0)
    (progn
     (setf
      (ppu-tv p)
      (wrap-word
      (logior
       (logand
        (ppu-tv p)
        #xFFE0)
       (ash value -3))))
     (setf (ppu-x p) (logand value #x07))
     (setf (ppu-w p) 1))
    (progn
     (setf
      (ppu-tv p)
      (wrap-word
      (logior
       (logand
        (ppu-tv p)
        #x8FFF)
       (ash (logand value #x07) 12))))
     (setf
      (ppu-tv p)
      (wrap-word
      (logior
       (logand (ppu-tv p) #xFC1F)
       (ash (logand value #xF8) 2))))
     (setf (ppu-w p) 0))))

(defun write-address (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
  (if (= (ppu-w p) 0)
    (progn
     (setf
      (ppu-tv p)
      (wrap-word
       (logior
        (logand (ppu-tv p) #x80FF)
        (ash (logand value #x3F) 8))))
     (setf (ppu-w p) 1))
    (progn
     (setf
      (ppu-tv p)
      (logior (logand (ppu-tv p) #xFF00) value))
     (setf (ppu-v p) (ppu-tv p))
     (setf (ppu-w p) 0))))


(defun read-data (p)
  (declare (ppu p))
  (let ((value (read-ppu p (ppu-v p))))
    (declare ((unsigned-byte 8) value))
    (if (< (mod (ppu-v p) #x4000) #x3F00)
      (progn
       (let ((buffered (ppu-buffered-data p)))
         (setf (ppu-buffered-data p) value)
         (setf value buffered)))
      (setf
       (ppu-buffered-data p)
       (read-ppu p (wrap-word (- (ppu-v p) #x1000)))))
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
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
  (write-ppu p (ppu-v p) value)
  (setf
   (ppu-v p)
   (wrap-word
    (+
     (ppu-v p)
     (if (= (ppu-flag-increment p) 0)
       1
       32)))))

(defun write-dma (p value)
  (declare (ppu p))
  (declare ((unsigned-byte 8) value))
  (let ((address (wrap-word (ash value 8))))
    (loop for i from 0 to 255
      do
      (progn
       (setf
        (aref (ppu-oam-data p) (ppu-oam-address p))
        (funcall (ppu-oam-dma-callback p) address))
       (setf (ppu-oam-address p) (wrap-byte (1+ (ppu-oam-address p))))
       (setf address (wrap-word (1+ address)))))
    (funcall (ppu-oam-stall-adder p) 513)))

(defun read-register (p selector)
  (declare (ppu p))
  (declare ((unsigned-byte 16) selector))
  (case selector
    ;Read ppu status
    (2 (read-status p))
    ;Read OAM Data
    (4 (read-oam-data p))
    ;Read Data
    (7 (read-data p))
    (otherwise (progn (print "Uhm?") 0))))

(defun write-register (p selector value)
  (declare (ppu p))
  (declare ((unsigned-byte 16) selector))
  (setf (ppu-register p) value)
  (case selector
    ;Write Control
    (0 (write-control p value))
    ;Write Mask
    (1 (write-mask p value))
    ;Write OAM Address
    (3 (write-oam-address p value))
    ;Write OAM Data
    (4 (write-oam-data p value))
    ;Write scroll
    (5 (write-scroll p value))
    ;Write Address
    (6 (write-address p value))
    ;Write Data
    (7 (write-data p value))
    ;Write DMA
    (#x4014 (write-dma p value))
    (otherwise (progn (print "Uhm in write?") 0))))

(defun increment-x (p)
  (declare (ppu p))
  (if (= (logand (ppu-v p) #x001F) 31)
    (setf (ppu-v p) (wrap-word (logxor #x0400 (logand (ppu-v p) #xFFE0)))))
    (setf (ppu-v p) (wrap-word (1+ (ppu-v p)))))

(defun increment-y (p)
  (declare (ppu p))
  (if (not (= (logand (ppu-v p) #x7000) #x7000))
    (setf (ppu-v p) (wrap-word (+ #x1000 (ppu-v p))))
    (progn
     (setf (ppu-v p) (logand (ppu-v p) #x8FFF))
     (let ((y (ash (logand (ppu-v p) #x03E0) -5)))
       (if (= y 29)
         (progn
          (setf y 0)
          (setf (ppu-v p) (logxor (ppu-v p) #x0800)))
         (if (= y 31)
           (setf y 0)
           (incf y)))
       (setf (ppu-v p) (wrap-word (logior (logand (ppu-v p) #xFC1F) (ash y 5))))))))

(defun copy-x (p)
  (declare (ppu p))
  (setf (ppu-v p) (logior (logand (ppu-v p) #xFBE0) (logand (ppu-tv p) #x041F))))

(defun copy-y (p)
  (declare (ppu p))
  (setf (ppu-v p) (logior (logand (ppu-v p) #x841F) (logand (ppu-tv p) #x7BE0))))

(defun nmi-change (p)
  (declare (ppu p))
  (let ((nmi (and (ppu-nmi-output p) (ppu-nmi-occurred p))))
    (when (and nmi (not (ppu-nmi-previous p)))
      (setf (ppu-nmi-delay p) 15))
    (setf (ppu-nmi-previous p) nmi)))

(defun set-vertical-blank (p)
  (declare (ppu p))
  (psetf (ppu-front p) (ppu-back p) (ppu-back p) (ppu-front p))
  (setf (ppu-nmi-occurred p) T)
  (nmi-change p))

(defun clear-vertical-blank (p)
  (declare (ppu p))
  (setf (ppu-nmi-occurred p) nil)
  (nmi-change p))

(defun fetch-name-table (p)
  (declare (ppu p))
  (let* ((v (ppu-v p))
         (address (logior #x2000 (logand v #x0FFF))))
    (setf (ppu-name-table p) (read-ppu p address))))

(defun fetch-attribute-table (p)
  (declare (ppu p))
  (let* ((v (ppu-v p))
         (address
          (wrap-word
           (logior
            #x23C0
            (logand v #x0C00)
            (logand #x38 (ash v -4))
            (logand #x07 (ash v -2)))))
         (shift (logior (logand (ash v -4) 4) (logand v 2))))
    (setf
     (ppu-attribute-table p)
     (wrap-byte
      (ash (logand (ash (the (unsigned-byte 8) (read-ppu p address)) (* -1 shift)) 3) 2)))))

(defun fetch-low-tile (p)
  (declare (ppu p))
  (let* ((fine-y (logand 7 (ash (ppu-v p) -12)))
         (table (ppu-flag-background-table p))
         (tile (ppu-name-table p))
         (address (+ (* #x1000 (logand #xFFFF table)) (* 16 (logand #xFFFF tile)) fine-y)))
    (setf (ppu-low-tile p) (read-ppu p address))))

(defun fetch-high-tile (p)
  (declare (ppu p))
  (let* ((fine-y (logand 7 (ash (ppu-v p) -12)))
         (table (ppu-flag-background-table p))
         (tile (ppu-name-table p))
         (address (+ (* #x1000 (logand #xFFFF table)) (* 16 (logand #xFFFF tile)) fine-y)))
    (setf (ppu-high-tile p) (read-ppu p (wrap-word (+ address 8))))))

(defun store-tile-data (p)
  (declare (ppu p))
  (let ((data 0))
    (declare ((unsigned-byte 32) data))
    (loop for i from 0 to 7
      do
      (let ((a (ppu-attribute-table p))
            (p1 (ash (logand #x80 (ppu-low-tile p)) -7))
            (p2 (ash (logand #x80 (ppu-high-tile p)) -6)))
        (declare ((unsigned-byte 8) a p1 p2))
        (setf (ppu-low-tile p) (wrap-byte (ash (ppu-low-tile p) 1)))
        (setf (ppu-high-tile p) (wrap-byte (ash (ppu-high-tile p) 1)))
        (setf data (ash data 4))
        (setf
         data
         (logior a p1 p2 data))))
    (the (unsigned-byte 64) (setf (ppu-tile-data p) (logior (ppu-tile-data p) data)))))

(defun fetch-tile-data (p)
  (declare (ppu p))
  (the (unsigned-byte 32) (logand
   #xFFFFFFFF
   (ash
    (ppu-tile-data p)
    -32))))

(defun background-pixel (p)
  (declare (ppu p))
  (if (= (ppu-flag-show-background p) 0)
    0
    (logand
     #x0F
     (ash
      (fetch-tile-data p)
      (* (- 7 (ppu-x p)) 4 -1)))))

(defun sprite-pixel (p)
  (declare (ppu p))
  (when (= (ppu-flag-show-sprites p) 0)
    (return-from sprite-pixel (list 0 0)))
  (loop for i from 0 to (- (ppu-sprite-count p) 1)
    do
    (progn
     (let ((offset (- (- (ppu-cycle p) 1) (aref (ppu-sprite-positions p) i))))
       (when (= (ppu-flag-show-sprites p) 0)
         (return-from sprite-pixel (list 0 0)))
       (when (and (>= offset 0) (<= offset 7))
         (setf offset (- 7 offset))
         (let (
               (color
                (wrap-byte
                 (logand
                  #x0F
                  (ash
                   (aref (ppu-sprite-patterns p) i)
                   (* -1 (wrap-byte (* offset 4))))))))
           (when (not (= (mod color 4) 0))
             (return-from sprite-pixel (list (wrap-byte i) color))))))))
  (return-from sprite-pixel (list 0 0)))

(defun render-pixel (p)
  (declare (ppu p))
  (destructuring-bind
   (i sprite)
   (sprite-pixel p)
   (declare ((unsigned-byte 8) i sprite))
   (let ((x (- (ppu-cycle p) 1))
        (y (ppu-scanline p))
        (background (background-pixel p)))
     (when (and (< x 8) (= (ppu-flag-show-left-background p) 0))
       (setf background 0))
     (when (and (< x 8) (= (ppu-flag-show-left-sprites p) 0))
       (setf sprite 0))
     (let ((b (not (= (mod background 4) 0)))
           (s (not (= (mod sprite 4) 0)))
           (color #x00))
       (cond
         ((and (not b) (not s)) (setf color 0))
         ((and (not b) s) (setf color (logior #x10 sprite)))
         ((and b (not s)) (setf color background))
         (T
          (progn
           (when (and (< x 255) (= (aref (ppu-sprite-indexes p) i) 0))
             (setf (ppu-flag-sprite-zero-hit p) 1))
           (if (= (aref (ppu-sprite-priorities p) i) 0)
             (setf color (logior sprite #x10))
             (setf color background)))))
       (setf
        (aref (ppu-back p) (+ (* y screen-width) x))
        (aref
         (the (simple-array color 1) *palette*)
         (mod (read-palette p (logand #xFFFF color)) 64)))))))

(defun fetch-sprite-pattern (p i r)
  (declare (ppu p))
  (declare ((signed-byte 16) i r))
  (let* ((tile (aref (ppu-oam-data p) (1+ (* i 4))))
        (attributes (aref (ppu-oam-data p) (+ 2 (* i 4))))
        (address #x0000)
        (a (ash (logand attributes 3) 2))
        (row r))
    (declare (fixnum row))
    (if (= (ppu-flag-sprite-size p) 0)
      (progn
       (when (= (logand attributes #x80) #x80)
         (setf row (- 7 row)))
       (let ((table (ppu-flag-sprite-table p)))
         (setf
          address
          (+
           (logand #xFFFF (* #x1000 (logand #xFFFF table)))
           (logand #xFFFF (* 16 (logand #xFFFF tile)))
           (logand #xFFFF row)))))
      (progn
       (when (= (logand attributes #x80) #x80)
         (setf row (- 15 row)))
       (let ((table (logand tile 1)))
         (setf tile (logand tile #xFE))
         (when (> row 7)
           (incf tile)
           (decf row 8))
         (setf
          address
          (+
           (* #x1000 (logand #xFFFF table))
           (* (logand #xFFFF tile) 16)
           (logand #xFFFF row))))))
    (let ((low-tile (read-ppu p address))
          (high-tile (read-ppu p (wrap-word (+ address 8))))
          (data #x00000000))
      (declare ((unsigned-byte 8) low-tile high-tile))
      (loop for i from 0 to 7
        do
        (let ((p1 0)
              (p2 0))
          (if (= (logand attributes #x40) #x40)
            (progn
             (setf p1 (logand low-tile 1))
             (setf p2 (wrap-byte (ash (logand high-tile 1) 1)))
             (setf low-tile (wrap-byte (ash low-tile -1)))
             (setf high-tile (wrap-byte (ash high-tile -1))))
            (progn
             (setf p1 (wrap-byte (ash (logand low-tile #x80) -7)))
             (setf p2 (wrap-byte (ash (logand high-tile #x80) -6)))
             (setf low-tile (wrap-byte (ash low-tile 1)))
             (setf high-tile (wrap-byte (ash high-tile 1)))))
          (setf data (logand #xFFFFFFFF (ash data 4)))
          (setf data (logand #xFFFFFFFF (logior data a p1 p2)))))
      (the (unsigned-byte 32) data))))

(defun evaluate-sprites (p)
  (declare (ppu p))
  (let ((h
         (if (= (ppu-flag-sprite-size p) 0)
           8
           16))
        (count 0))
    (declare (fixnum count))
    (declare ((unsigned-byte 8) h))
    (loop for i from 0 to 63
      do
      (progn
       (let* ((y (aref (ppu-oam-data p) (* i 4)))
             (a (aref (ppu-oam-data p) (+ 2 (* i 4))))
             (x (aref (ppu-oam-data p) (+ 3 (* i 4))))
             (row (- (ppu-scanline p) y)))
         (when (and (>= row 0) (< row h))
           (progn
           (when (< count 8)
             (setf
              (aref (ppu-sprite-patterns p) count)
              (fetch-sprite-pattern p i row))
             (setf
              (aref (ppu-sprite-positions p) count)
              x)
             (setf
              (aref (ppu-sprite-priorities p) count)
              (logand 1 (ash a -5)))
             (setf
              (aref (ppu-sprite-indexes p) count)
              (wrap-byte i)))
           (incf count))))))
    (when (> count 8)
      (setf count 8)
      (setf (ppu-flag-sprite-overflow p) 1))
    (setf (ppu-sprite-count p) count)))

(defun reset-ppu (p)
  (declare (ppu p))
  (setf (ppu-cycle p) 340)
  (setf (ppu-scanline p) 240)
  (setf (ppu-frame p) 0)
  (write-control p 0)
  (write-mask p 0)
  (write-oam-address p 0))

(defun tick (p)
  (declare (ppu p))
  ;While the nmi delay is greater than zero...
  (when (> (ppu-nmi-delay p) 0)
    ;Decrement it...
    (decf (ppu-nmi-delay p))
    ;Trigger it if we have run out of time and there even is one
    (when (and (= (ppu-nmi-delay p) 0) (ppu-nmi-output p) (ppu-nmi-occurred p))
      (funcall (ppu-trigger-nmi-callback p))))
  (when
    (or (not (= 0 (ppu-flag-show-background p))) (not (= 0 (ppu-flag-show-sprites p))))
    (when (and
           (= (ppu-f p) 1)
           (= (ppu-scanline p) 261)
           (= (ppu-cycle p) 339))
      (setf (ppu-cycle p) 0)
      (setf (ppu-scanline p) 0)
      (setf (ppu-frame p) (wrap-word (1+ (ppu-frame p))))
      (setf (ppu-f p) (logxor (ppu-f p) 1))
      (return-from tick 0)))
  (incf (ppu-cycle p))
  ;We've hit the end of the scanline
  (when (> (ppu-cycle p) 340)
    ;Reset the cycle number
    (setf (ppu-cycle p) 0)
    ;Increment the scanline
    (incf (ppu-scanline p))
    ;And finally do housework if we need to go back to top
    (when (> (ppu-scanline p) 261)
      (setf (ppu-scanline p) 0)
      (setf (ppu-frame p) (wrap-word (1+ (ppu-frame p))))
      (setf (ppu-f p) (logxor (ppu-f p) 1)))))

(defun step-ppu (p step)
  (declare (ppu p) ((unsigned-byte 8) step))
  (loop for i from 1 to step
    do
    (progn
     (tick p)
     (let* ((cycle (ppu-cycle p))
            (scanline (ppu-scanline p))
            (rendering-enabled
             (not
              (=
               0
               (ppu-flag-show-background p)
               (ppu-flag-show-sprites p))))
            (pre-line (= scanline 261))
            (visible-line (< scanline 240))
            (render-line (or pre-line visible-line))
            (pre-fetch-cycle (and (>= cycle 321) (<= cycle 336)))
            (visible-cycle (and (>= cycle 1) (<= cycle 256)))
            (fetch-cycle (or pre-fetch-cycle visible-cycle)))
       (when rendering-enabled
         ;Begin background logic
         (when (and visible-line visible-cycle)
           (render-pixel p))
         ;When we are on a fetch cycle and a render line...
         (when (and render-line fetch-cycle)
           ;Shift tile data left four to make room
           (setf
            (ppu-tile-data p)
            ;Make sure it continues to fit in 64 bits
            (logand
             #xFFFFFFFFFFFFFFFF
             (ash (ppu-tile-data p) 4)))
           ;Depending on what cycle we are in act accordingly
           (case (mod cycle 8)
             (0 (store-tile-data p))
             (1 (fetch-name-table p))
             (3 (fetch-attribute-table p))
             (5 (fetch-low-tile p))
             (7 (fetch-high-tile p))))
         ;When we are on preline and
         (when (and pre-line (>= (ppu-cycle p) 280) (<= (ppu-cycle p) 304))
           (copy-y p))
         (when render-line
           (when (and fetch-cycle (= (mod cycle 8) 0))
             (increment-x p))
           (when (= cycle 256)
             (increment-y p))
           (when (= cycle 257)
             (copy-x p)))
         ;begin sprite logic
         (when (= cycle 257)
           (if visible-line
             (evaluate-sprites p)
             (setf (ppu-sprite-count p) 0))))
       ;Begin vblank logic
       (when (and (= scanline 241) (= (ppu-cycle p) 1))
         (set-vertical-blank p))
       (when (and pre-line (= (ppu-cycle p) 1))
         (clear-vertical-blank p)
         (setf (ppu-flag-sprite-zero-hit p) 0)
         (setf (ppu-flag-sprite-overflow p) 0))))))
