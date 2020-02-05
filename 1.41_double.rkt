#lang racket
(require "math.rkt")

(define (double f)
  (lambda (x)
    (f (f x))))

((double inc) 1)
(((double (double double)) inc) 5)