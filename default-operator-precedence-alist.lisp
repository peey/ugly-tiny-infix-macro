(in-package :ugly-tiny-infix-macro)
;; default list of binary operations in the order of precedence, taken in the order that C++ takes it in sans lognor, logeqv and other binary operators unavailable in C++
;; , see http://clhs.lisp.se/Body/f_logand.htm
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
    (logand . 10)
    (logxor . 11)
    (logior . 12) ; what's the i for? I hope this is same as cpp bitwise or
    (and . 13)
    (or . 14)))

;; unsure if I should add the following, they apparently serve as a way of division, see http://www.lispworks.com/documentation/HyperSpec/Body/f_floorc.htm
;;    (floor . 5)
;;    (ffloor . 5)
;;    (ceiling . 5)
;;    (fceiling . 5)
;;    (truncate . 5)
;;    (ftruncate . 5)
;;    (round . 5)
;;    (fround . 5)
