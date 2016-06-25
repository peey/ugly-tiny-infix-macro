(asdf:defsystem ugly-tiny-infix-macro
  :name "ugly-tiny-infix-macro"
  :description "A tiny and simple macro to allow writing binary operations in infix notation"
  :author "Peeyush Kushwaha <peeyush.p97+dev@gmail.com>"
  :license "Apache License, Version 2.0"
  :serial t
  :components ((:file "defpackage")
	       (:file "default-operator-precedence-alist")
	       (:file "error-handling")
               (:file "ugly-tiny-infix-macro")))
