#lang racket
(require "math.rkt")

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

;;test code
(sum-cubes 1 10)
(sum-integers 1 100)