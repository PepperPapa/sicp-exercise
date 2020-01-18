#lang racket

(provide square)
(provide cube)
(provide expt)

(define (square x) (* x x))
(define (cube x) (* x (* x x)))
(define (expt b n)
  (define (fast-expt-iter b n a)
    (cond ((< n 1) a)
          ((even? n)
           (display '+)
           (fast-expt-iter (square b) (/ n 2) a))
          (else
           (display '+)
           (fast-expt-iter (square b) (/ (- n 1) 2) (* a b)))))
  (fast-expt-iter b n 1))