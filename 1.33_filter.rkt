#lang racket
(require "prime.rkt")
(require "math.rkt")

(define (filtered-accumulate combiner null-value term a next b valid?)
  (define (iter a result)
    (cond ((> a b)
           result)
          ((valid? (term a))
           (iter (next a)
                 (combiner result (term a))))
          (else
           (iter (next a) result))))
  (iter a null-value))

(define (filter valid? items)
  (cond ((not (null? items))
         (if (valid? (car items))
             (cons (car items)
                   (filter valid? (cdr items)))
             (filter valid? (cdr items))))
        (else
         '())))

(define (sum-prime a b)
  (filtered-accumulate + 0 identity a inc b prime?))

(define (product-of-coprime n)
  (define (coprime a)
    (if (and (= 1 (gcd a n)) (< a n))
        #t
        #f))
  (filtered-accumulate * 1 identity 1 inc n coprime))

; test cases for filter
(filter prime? (list 1 2 3 4 5 6 7 8 9 10 11 12 13))
(sum-prime 1 10)
(sum-prime 3 10)
(product-of-coprime 10)