(in-package :cl-user)

(defpackage #:NES-cartridge
  (:nicknames #:nes-cart)
  (:use :cl :cl-user)
  (:export #:load-cartridge #:make-cartridge #:cartridge-prg-rom
           #:cartridge-prg-ram #:cartridge-chr-rom #:cartridge-chr-ram
           #:cartridge-mirror))

(in-package :NES-cartridge)
(defconstant prg-size #x4000)
(defconstant chr-size #x2000)

(defstruct ines-header
  "ines header spec"
  (magic (make-array 4 :element-type '(unsigned-byte 8)))
  ;Size of prg rom in 16 KiB units
  (size-of-prg-rom 0 :type (unsigned-byte 8))
  ;Size of chr rom in 8 KiB units
  (size-of-chr-rom 0 :type (unsigned-byte 8))
  (flags6 0 :type (unsigned-byte 8))
  (flags7 0 :type (unsigned-byte 8))
  ;Size of PRG ram in 8 KiB unites
  (size-of-prg-ram 0 :type (unsigned-byte 8))
  (flags9 0 :type (unsigned-byte 8))
  (flags10 0 :type (unsigned-byte 8))
  (zero-pad (make-array 5 :element-type '(unsigned-byte 8))))

(defstruct cartridge
  "A model NES cartridge"
  (prg-rom 0)
  (prg-rom-window 0 :type (unsigned-byte 8))
  ;We ignore prg-ram for now
  (prg-ram 0)
  (prg-ram-window 0 :type (unsigned-byte 8))
  (chr-rom 0)
  (chr-ram 0)
  (chr-window 0 :type (unsigned-byte 8))
  (mapper-number 0)
  (header 0)
  (mirror))

(defun load-header (seq)
  (make-ines-header
   :magic (subseq seq 0 4)
   :size-of-prg-rom (aref seq 4)
   :size-of-chr-rom (aref seq 5)
   :flags6 (aref seq 6)
   :flags7 (aref seq 7)
   :size-of-prg-ram (aref seq 8)
   :flags9 (aref seq 9)
   :flags10 (aref seq 10)
   :zero-pad (subseq seq 11 16)))

(defun load-cartridge (filepath)
  (let ((cart (make-cartridge)) (header (make-ines-header)))
    (with-open-file (stream filepath :element-type '(unsigned-byte 8))
      (let ((seq
             (make-array
              (file-length stream)
              :element-type '(unsigned-byte 8))))
        (read-sequence seq stream)
        (setf header (load-header seq))
        (let*
          ;If trainers are present, skip them.
          ((to-add
            (if (= (logand (ash (ines-header-flags6 header) -2) 1) 1)
              512
              0))
           ;Limits of memory areas
           (begin-prg (+ 16 to-add))
           (end-prg (+ begin-prg (* prg-size (ines-header-size-of-prg-rom header))))
           (begin-chr end-prg)
           (end-chr (+ begin-chr (* chr-size (ines-header-size-of-chr-rom header)))))
          ;Load in prg-rom
          (setf
           (cartridge-prg-rom cart)
           (subseq
            seq
            begin-prg
            end-prg))
          ;If there is no rom, there is ram
          (if (= (ines-header-size-of-chr-rom header) 0)
            (setf
             (cartridge-chr-ram cart)
             (make-array chr-size :element-type '(unsigned-byte 8)))
            (setf
             (cartridge-chr-rom cart)
             (subseq seq begin-chr end-chr)))
          (setf (cartridge-header cart) header)
          (let ((mirror1 (logand (ines-header-flags6 header) 1))
                (mirror2 (logand (ash (ines-header-flags6 header) -3) 1)))
                (setf
                 (cartridge-mirror cart)
                 (logior mirror1 (ash mirror2 1))))))
    cart)))
