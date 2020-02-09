#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

;;牛顿法求平方根
;;y^2 = x,y为x的平方根
;;有了一个猜测值y，则(y+x/y)/2更接近实际的平方根值
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) 1.0))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))


;;test case
(sqrt 4)
(sqrt 81)
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)