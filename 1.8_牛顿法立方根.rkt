#lang sicp

(define (square x)
  (* x x))

(define (good-enough? previous-guess guess x)
  (< (abs (/ (- previous-guess guess) guess)) 0.001))

;;相对于平方根和立方根，只需要改变improve，其他都不需要改变
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (cubic-iter previous_guess guess x)
  ;(display guess)
  ;(newline)
  (if (good-enough? previous_guess guess x)
      guess
      (cubic-iter guess (improve guess x) x)))

(define (cubic x)
  (cubic-iter 0.0 1.0 x))

;;test code
(cubic 0.00000000001)
(cubic 1)
(cubic 8)
(cubic 512)
(cubic 999999999999999999999)