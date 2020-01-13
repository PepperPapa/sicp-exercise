#lang racket

;;练习1.6 为什么需要将if提供为一种特殊形式？
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;牛顿法求平方根
(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

;;new-if之所以会陷入死循环，是因为cond方式每次执行不管结果真假在返回结果前都首先去执行then-clause和else-clause
;;而else-clause本身就是调用sqrt-iter自身，所以永远都不可能存在结束条件。
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;test code
;(new-if (= 2 3) 0 5)
;(new-if (= 1 1) 0 5)
(sqrt 2)
