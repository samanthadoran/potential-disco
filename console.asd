(asdf:defsystem #:console
  :components (
               (:file "console"
		             :depends-on ("cpu" "cartridge"))
               (:file "cpu")
               (:file "cartridge")
               (:file "instructions/arithmeticops")
               (:file "instructions/branchops")
               (:file "instructions/flagops")
               (:file "instructions/instructions")))
