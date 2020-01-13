#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

;;牛顿法求平方根
;;y^2 = x,y为x的平方根
;;有了一个猜测值y，则(y+x/y)/2更接近实际的平方根值
(define (sqrt-newton x)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; test cases
(sqrt-newton 0.00001)   ;;不准确
(sqrt-newton 2)
(sqrt-newton 4)
(sqrt-newton 9)
(sqrt-newton 16)
