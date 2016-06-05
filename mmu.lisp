(in-package :NES-console)

(declaim (optimize (speed 3) (safety 1)))

(defun mirror-address (mode addr)
  (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) mode))
  (let* ((address (mod (- addr #x2000) #x1000))
        (table (floor address #x0400))
        (offset (mod address #x0400)))
    (declare ((unsigned-byte 16) address table offset))
    (logand #xFFFF (+ #x2000 offset (* #x0400 (the (unsigned-byte 3) (aref (the (simple-array (unsigned-byte 2) 1)mirror-lookup) (+ (* mode 4) table))))))))

(defun ppu-to-name-table-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (aref
           (the (simple-array (unsigned-byte 8) 1) (NES-ppu:ppu-name-table-data (nes-ppu n)))
           (logand (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) #x7ff))))

(defun ppu-to-name-table-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1) (NES-ppu:ppu-name-table-data (nes-ppu n)))
            (logand (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) #x7ff))
           val)))

(defun ppu-to-palette-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (NES-ppu:read-palette (nes-ppu n) (logand addr #x1f))))

(defun ppu-to-palette-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (NES-ppu:write-palette (nes-ppu n) (logand addr #x1f) val)))

(defun ppu-to-mapper-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (aref
            (if (arrayp (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-rom (nes-cart n))))
           addr)))

(defun ppu-to-mapper-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (setf
           (aref
            (if (arrayp (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-ram (nes-cart n)))
              (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-chr-rom (nes-cart n))))
            addr)
           val)))

(defun cpu-to-cpu-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (aref
           (the (simple-array (unsigned-byte 8) 1) (6502-cpu:cpu-memory (nes:nes-cpu n)))
           (mod
            addr
            #x800))))

(defun cpu-to-cpu-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1) (6502-cpu:cpu-memory (nes:nes-cpu n)))
            (mod
             addr
             #x800))
           val)))

(defun cpu-to-cart-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (aref
           (the (simple-array (unsigned-byte 8) 1) (NES-cartridge:cartridge-prg-rom (nes-cart n)))
           (mod
            addr
            (array-dimension (the (simple-array (unsigned-byte 8) 1)(NES-cartridge:cartridge-prg-rom (nes-cart n))) 0)))))

(defun cpu-to-cart-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (setf
           (aref
            (the (simple-array (unsigned-byte 8) 1)(NES-cartridge:cartridge-prg-rom (nes-cart n)))
            (mod
             addr
             (array-dimension (the (simple-array (unsigned-byte 8) 1)(NES-cartridge:cartridge-prg-rom (nes-cart n))) 0)))
           val)))

(defun cpu-to-ppu-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          ;If oam, don't mod the address
          (if (= addr #x4014)
            (NES-ppu:read-register (nes-ppu n) addr)
            (NES-ppu:read-register (nes-ppu n) (mod addr 8)))))
(defun cpu-to-ppu-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (if (= addr #x4014)
            (NES-ppu:write-register (nes-ppu n) addr val)
            (NES-ppu:write-register (nes-ppu n) (mod addr 8) val))))

(defun cpu-to-io-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (if (or (= addr #x4016) (= addr #x4017))
            (NES-controller:read-controller
             (aref
              (the (simple-array nes-controller:controller 1)(nes-controllers n))
              (mod addr 2)))
            0)))

(defun cpu-to-io-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (if (or (= addr #x4016) (= addr #x4017))
            (NES-controller:write-controller
             (aref
              (the (simple-array nes-controller:controller 1)(nes-controllers n))
              (mod addr 2))
             val)
            0)))
