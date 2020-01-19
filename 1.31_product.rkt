#lang racket
(require "math.rkt")

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (get-pi times)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* 4.0
     (product-iter term 1 inc times)))
           
;;test code
(get-pi 1)
(get-pi 2)
(get-pi 3)
(get-pi 4)
(get-pi 10)
(get-pi 100)
(get-pi 1000)
(get-pi 10000)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)