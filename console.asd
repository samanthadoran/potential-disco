(asdf:defsystem #:console
  :components ((:file "console"
		      :depends-on ("cpu" "cartridge"))
          (:file "cpu")
          (:file "cartridge")))
