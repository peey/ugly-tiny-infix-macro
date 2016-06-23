(defpackage :ugly-tiny-infix-macro
  (:use :common-lisp)
  (:export :$
	   :*operator-precedence-alist*
	   :malformed-infix-expression-error)
  (:nicknames :ugly-infix))
