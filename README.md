# Ugly Tiny Infix Macro

This is a powerful lisp macro for the purpose of writing your expressions in infix notation while not losing out on lisp's power.

Let's look at a few examples

## Examples

At its simplest,

    ($ 1 + 2)		; gets converted to (+ 1 2), where name of the macro is $
    ($ t and nil) 	; gets converted to (and t nil)
    ($ 3 > 5) 		; gets converted to (> 3 5)
    ($ 1 + 2 + 3) 	; gets converted to (+ (+ 1 2) 3)

You can use various operators (which is just a fancy name for a function) at once and they're grouped according to the precedence.

Default precedence of operators that ships with the project is taken from the [C++ standard](http://en.cppreference.com/w/cpp/language/operator_precedence). Check out the section on customizing the list of operators and their priorities if you wish to do so.

    ($ 1 + 2 *  3)      ; gets converted to (+ 1 (* 2 3))
    ($ 1 < 2 and 2 < 3) ; gets converted to (AND (< 1 2) (< 2 3))

Anything within parentheses at position of an operand is treated like a lisp form.

    ($ 2 + (max 9 10 11)) ; gets converted to (+ 2 (max 9 10 11)). It could have been any function / lisp form.
    ($ 6 / ($ 1 + 2))     ; gets converted to (/ 6 ($ 1 + 2)), and then subsequently to (/6 (+ 1 2))

As illustrated by the last example, nesting the macro can be used for "grouping" or "higher precedence" so that it's evaluated first like is done with brackets in mathematical notation.

You may write the last example as 6 / (1 + 2) in math. Most deeply nested brackets are executed the first, and same is the case when you nest this macro.

**WARNING**: As explained in the example on nesting the macro, brackets are assumed to be valid lisp forms. Do not use them for grouping. Writing `($ 6 / (1 + 2))` will give you an error as it's expanded to `(/ 6 (1 + 2))` and `(1 + 2)` is not a valid lisp form. Your lisp environment will try to evaluate it and find that `1` isn't name of a function that can be called with the arguments `+` and `2`. This is what puts "ugly" in the the name of the project, because it's not prettiest to the eye, but some may find that it's easy to reason about.

# Usage

The package, named `:ugly-tiny-lisp-macro` with the nickname `:ugly-infix` exports three symbols -

 - $ : this is the macro itself
 - *operator-precedence-alist* : An alist of operators (lisp functions) and their priorities
 - malformed-infix-expression-error : A condition which is signaled when something other than the operators in `*operator-precedence-alist* is found at even positions in the expression, or if the expression length is not an odd number.
 
## Customizing The List of Operators and Their Priorities

Operator precedence is stored as an alist associated with the symbol `*operator-precedence-alist*`. An example of a valid alist that one may assign for DMAS precedence may look like:

    (setf ugly-infix:*operator-precedence-alist* 
    	  '(( / . 1) ; a lower number means a higher priority/precedence
	    ( * . 1) ; / and * are at the same priority/precedence
	    ( + . 2) ; a higher number means a lower priority/precedence
	    ( - . 2)))

You may modify `*operator-precedence-alist*` in any manner by resetting, pushing, etc as long as it is a valid alist of operators and their priority. The position in the list / order of cons elements does not matter.

## Miscellaneous 

 Note that this project only deals with binary operators, e.g. functions that accept (operate on) two arguments.
 
# License

This project is licensed under the terms of **APACHE 2.0** license. Please see the `LICENSE` file for the text of the license.