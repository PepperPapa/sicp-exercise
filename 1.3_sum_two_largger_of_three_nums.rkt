#lang racket

;;返回三个数中两个较大的数之和
(define (sum_two_largger a b c)
  (if (>= a b) (+ a (max b c))
      (+ b (max a c))))

;; test cases
(sum_two_largger 1 2 3)
(sum_two_largger 9 (+ 9 9) 20)
(sum_two_largger 90 99 100)
