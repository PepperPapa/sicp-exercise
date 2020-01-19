#lang racket
(require "math.rkt")

;;进一步抽象出accumulate的意义是什么？
;;如果accumulate已经经过充分测试，认为是成熟的过程，那么sum、product过程都利用了accumulate
;;方法，sum和product需要分别测试的部分就相对较少，可靠性也比分别独立实现更高
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))
  
(define (sum-integers a b)
  (sum identity a inc b))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iter identity 1 inc n))

;; test code
(sum-integers 1 100)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)