#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; test cases
(gcd 0 0)
(gcd 2 0)
(gcd 206 40)