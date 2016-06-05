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
           (NES-ppu:ppu-name-table-data (nes-ppu n))
           (logand (mirror-address (NES-cartridge:cartridge-mirror (nes-cart n)) addr) #x7ff))))

(defun ppu-to-name-table-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (setf
           (aref
            (NES-ppu:ppu-name-table-data (nes-ppu n))
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
          (aref (6502-cpu:cpu-memory (nes:nes-cpu n)) (mod addr #x800))))

(defun cpu-to-cpu-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (setf
           (aref (6502-cpu:cpu-memory (nes:nes-cpu n)) (mod addr #x800))
           val)))

(defun cpu-to-cart-read (n)
  (lambda (addr)
          (declare ((unsigned-byte 16) addr) (nes n))
          (let ((prg (NES-cartridge:cartridge-prg-rom (nes-cart n))))
            (declare ((simple-array (unsigned-byte 8) 1) prg))
            (aref prg (mod addr (array-dimension prg 0))))))

(defun cpu-to-cart-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (let ((prg (NES-cartridge:cartridge-prg-rom (nes-cart n))))
            (declare ((simple-array (unsigned-byte 8) 1) prg))
            (setf (aref prg (mod addr (array-dimension prg 0))) val))))

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
             (aref (nes-controllers n) (mod addr 2)))
            0)))

(defun cpu-to-io-write (n)
  (lambda (addr val)
          (declare ((unsigned-byte 16) addr) ((unsigned-byte 8) val) (nes n))
          (if (or (= addr #x4016) (= addr #x4017))
            (NES-controller:write-controller
             (aref (nes-controllers n) (mod addr 2))
             val)
            0)))


(defun map-memory (n)
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
  (setf
   (NES-controller:controller-buttons-callback
    (aref
     (the (simple-array NES-controller:controller 1)(nes-controllers n))
     0))
   #'get-buttons))
