(in-package :ugly-tiny-infix-macro)
;; default list of binary operations in the order of precedence, taken in the order that C++ takes it in sans lognor, logeqv and other binary operators unavailable in C++
;; , see http://clhs.lisp.se/Body/f_bt_and.htm
;; reference used: http://en.cppreference.com/w/cpp/language/operator_precedence
;; this is an exported symbol, and may be changed/reset by the user
(defparameter *operator-precedence-alist*
  '((* . 5)
    (/ . 5)
    (mod . 5)
    (rem . 5)
    (+ . 6)
    (- . 6)
    (ash . 7) ; bitshift
    (< . 8)
    (<= . 8)
    (> . 8)
    (>= . 8)
    (= . 9)
    (/= . 9)
    (eq . 9) ; for checking boolean equality
    (eql . 9) ; more ways for checking equality and returning a boolean
    (equal . 9)
    (bit-and . 10)
    (bit-xor . 11)
    (bit-ior . 12) ; ior = inclusive or = same as cpp's bitwise or
    ;bit-nor and bit-nand are available but I'm unsure where to put them in this list
    (and . 13)
    (or . 14)))

;; unsure if I should add the following, they apparently serve as a way of division, but the second argument is optional. But the same can be said for the / function, that the second argument is optional
;; see http://www.lispworks.com/documentation/HyperSpec/Body/f_floorc.htm
;;    (floor . 5)
;;    (ffloor . 5)
;;    (ceiling . 5)
;;    (fceiling . 5)
;;    (truncate . 5)
;;    (ftruncate . 5)
;;    (round . 5)
;;    (fround . 5)
