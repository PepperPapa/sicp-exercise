#lang racket
(require "math.rkt")

(provide smallest-divisor)
(provide prime?)
(provide fast-prime?)

;;最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (begin
        (display '+)
        (gcd b (remainder a b)))))

;;寻找因子
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else
         (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

;;费马小定理
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else
         #f)))

;(fast-prime? 11497 10000)

; (prime? 2)
; (prime? 1)
; (prime? 3)
; (prime? 4)