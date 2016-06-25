(in-package :ugly-tiny-infix-macro)

(define-condition malformed-infix-expression-error (error)
  ((text :initarg :text :reader malformed-infix-expression-error-text)
   (expression :initarg :expression :reader malformed-infix-expression-error-expression)))


;; from http://stackoverflow.com/a/7382977/1412255
(defmethod print-object ((err malformed-infix-expression-error) ostream)
  (print-unreadable-object (err ostream :type t)
    (format ostream "~s" (malformed-infix-expression-error-text err))
    (fresh-line ostream)
    (format ostream "Offending Expression: ~a" (malformed-infix-expression-error-expression err))))

;; for error checking
(defun check-list-of-expressions (list-of-expressions operator-precedence-alist)
  (if (evenp (length list-of-expressions))
      (error 'malformed-infix-expression-error :text "Expression has an even length" :expression list-of-expressions))
  (if (not (loop for i from 1 below (length list-of-expressions) by 2
	      always (not (null (assoc (nth i list-of-expressions) operator-precedence-alist)))))
      (error 'malformed-infix-expression-error :text "Not every element at odd index (even positions) in expression is a binary operator present in given *operator-precedence-alist*"  :expression list-of-expressions)))

(defun check-operator-precedence-alist (operator-precedence-alist)
  (if (not (and
	    (listp operator-precedence-alist)
	    (not (null operator-precedence-alist))
	    (loop for item in operator-precedence-alist
	       always (and (consp item) (symbolp (car item)) (numberp (cdr item))))))
      (error 'type-error :expected-type "non-empty-alist" :datum operator-precedence-alist)))
      
