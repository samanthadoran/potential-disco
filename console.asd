(asdf:defsystem #:console
  :components (
               (:file "cpu")
               (:file "console"
		             :depends-on ("cpu" "cartridge" "ppu"))
               (:file "ppu")
               (:file "cartridge")
               (:file "instructions/arithmeticops")
               (:file "instructions/branchops")
               (:file "instructions/flagops")
               (:file "instructions/loadstoreops")
               (:file "instructions/instructions")))
