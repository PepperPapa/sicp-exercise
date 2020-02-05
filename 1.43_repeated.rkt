#lang racket
(require "math.rkt")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (cond ((= n 1)
         f)
        ((> n 1)
         (compose f (repeated f (- n 1))))
        (else
         (error "ERROR: invalid range for n!"))))

;; test case
((repeated square 2) 5)
((repeated square 1) 5)
((repeated square 0) 5)
  