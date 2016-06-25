(in-package :ugly-tiny-infix-macro)
    
;; returns values of  stack, list of popped elements
(defun recursively-pop-stack (stack element operator-precedence-alist &optional (popped '()) )
  (if (and (not (null stack))
	   (>=  (cdr (assoc element operator-precedence-alist)) (cdr (assoc (first stack) operator-precedence-alist))))
      ;; recursively pop stack
      (progn
	(setf popped (append popped (list (pop stack))))  ;; append to popped, not push, because order of returned popped stack should be same as was in working stack
	(recursively-pop-stack stack element operator-precedence-alist popped))
      ;; else
      (values (push element stack) popped)))

;; apply popped operators to first two operands in queue for each popped element (on top (index 0) of the stack should be the first operator to be applied)
(defun apply-popped (popped-stack queue)
  ;; example operation:
  ;;input      : popped-stack  = (* -), queue = (1 2 3)
  ;;iteration 1: popped-stack  = (-),   queue = ((* 2 1) 3)
  ;;iteration 2: popped-stack  = (),    queue = (- 3 (* 2 1))
  (loop for operator in popped-stack
     do
       (let ((b (pop queue))
	     (a (pop queue)))
	 (push (list operator a b) queue)))
  queue)

;; an implementation of shunting-yard algorithm for operators w/o parenthesis for grouping
(defun shunting-yard (list-of-expressions operator-precedence-alist)
  (let ((queue '())
	(stack '()))
    (loop for element in list-of-expressions
	 do
	 (if (assoc element operator-precedence-alist)
	     (multiple-value-bind (new-stack popped)
		 (recursively-pop-stack stack element operator-precedence-alist)
	       (setf stack new-stack)
	       (setf queue (apply-popped popped queue)))
	     ;; if number / something that's expected to evaluated to a number
	     (push element queue)))
    (first (apply-popped stack queue)))) ; append remaining stack to the queue, get the single expression left in the queue of expressions

(defmacro $ (&rest list-of-expressions)
  "Infix binary operations for lisp!"
  (check-operator-precedence-alist *operator-precedence-alist*)
  (check-list-of-expressions list-of-expressions *operator-precedence-alist*)
  (shunting-yard list-of-expressions *operator-precedence-alist*))
