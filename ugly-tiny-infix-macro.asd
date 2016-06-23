;(defsystem cow
;  ;;; (Optional items omitted)
;  :serial t ;; the dependencies are linear.
;  :components ((:file "1")))

;(asdf:defsystem dogf
;  ;;; (Optional items omitted)
;  :serial t ;; the dependencies are linear.
;  :components ((:file "default-operator-precedence-alist")))


(asdf:defsystem ugly-tiny-infix-macro
  :name "ugly-tiny-infix-macro"
  :description "A tiny and simple macro to allow writing binary operations in infix notation"
  :author "Peeyush Kushwaha <peeyush.p97+dev@gmail.com>"
  :license "Apache License, Version 2.0"
  :serial t
  :components ((:file "defpackage")
	       (:file "default-operator-precedence-alist")
               (:file "infix-macro")))
