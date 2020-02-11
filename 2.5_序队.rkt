#lang racket
(require rnrs/base-6)

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))

(define a (cons 6 4))
(display a)
(newline)
(car a)
(cdr a)