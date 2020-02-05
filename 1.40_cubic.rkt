#lang racket
(require "math.rkt")

(define dx 0.00001)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    ;(display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;g(x)=0的解转换为求f(x)=x-g(x)/Dg(x)的不动点
(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (result-of-cubic a b c)
  (newtons-method (cubic a b c) 1))

;; test case
;(sqrt 4)
;(sqrt 100)
(result-of-cubic 1 1 1)
(result-of-cubic 1 -10 1)