;; to use
;; 1. Load everything (if using slime use C-k)
;; 2. Switch to package :ugly-tiny-infix-macro-test (using (in-package :ugly-tiny-infix-macro-test))
;; 3. Run tests via (run-tests :all)

(asdf:load-asd (merge-pathnames #p"../ugly-tiny-infix-macro.asd" *LOAD-PATHNAME*))
(asdf:load-system :ugly-tiny-infix-macro)
(ql:quickload :lisp-unit)

(defpackage :ugly-tiny-infix-macro-test
  (:use :cl)
  (:use :ugly-tiny-infix-macro)
  (:use :lisp-unit))

(in-package :ugly-tiny-infix-macro-test)

(setf *operator-precedence-alist*
      (append *operator-precedence-alist*
	      '((O . 1) ; dummy operators for macroexpansion tests
		(O1 . 1)
		(O2 . 2)
		(O3 . 3))))

(define-test macro-expansion
  (assert-equal '(O a b) (macroexpand '($ a O b)))
  (assert-equal '(O (O a b) c) (macroexpand '($ a O b O c))))

(define-test operator-precedence
  (assert-equal '(O2 a (O1 b c)) (macroexpand '($ a O2 b O1 c)))
  (assert-equal '(O (O1 a b) c) (macroexpand '($ a O1 b O c)))
  (assert-equal '(O3 (O3 a (O2 b c)) (O1 d e)) (macroexpand '($ a O3 b O2 c O3 d O1 e))))

(define-test error-throwing
  (let ((original-value *operator-precedence-alist*))
    (unwind-protect ;; unwind-protect and reset the global variable *operator-precedence-alist*
	 (progn
	   (assert-equal :error-signalled (handler-case (macroexpand '($ a non-existent-op b))
					    (malformed-infix-expression-error () :error-signalled)))
	   (setf *operator-precedence-alist* :lol-not-an-alist-obviously)
	   (assert-equal :error-signalled (handler-case (macroexpand '($ 1 + 2))
					    (type-error () :error-signalled)))
	   (setf *operator-precedence-alist* '()) ;empty list should cause error too
	   (assert-equal :error-signalled (handler-case (macroexpand '($ 1 + 2))
					    (type-error () :error-signalled)))
	   (setf *operator-precedence-alist* '((+ . 1))) ;shouldn't be any errors for this case
	   (assert-equal 3 (handler-case ($ 1 + 2)
			     (type-error () :error-signalled)))))
    (setf *operator-precedence-alist* original-value)))
  


;; some with real numbers

(define-test nested-forms
  (assert-true (equal (macroexpand '($ 1 + (min 9 10))) '(+ 1 (min 9 10))))
  (assert-equal 10 ($ 1 + (min 9 10)))
  (assert-true (equal (macroexpand '($ ($ 9 - 6) / 3)) '(/ ($ 9 - 6) 3)))
  (assert-equal 1 ($ ($ 9 - 6) / 3)))

(define-test value-of-expressions
  (assert-equal 12 ($ 3 + 2 * 2 + 5))
  (assert-equal 8 ($ 3 * 2 + 2 * 1))
  (assert-equal -5 ($ 5 - 3 * 4 + 2))
  (assert-equal  4 ($ 5 - 3/4 + -1/4))
  (assert-equal  t ($ 5 > 4 and 9 > 8)))
