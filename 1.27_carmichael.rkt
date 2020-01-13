#lang racket
(require "math.rkt")

;;费马小定理
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n a)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it a))

(define (carmichael-test? n)
  (define (carmichael-iter n times)
    (cond ((= times 0) #t)
          ((fermat-test n times)
           ;(display times)
           ;(display " ")
           (carmichael-iter n (- times 1)))
          (else
           #f)))
  (carmichael-iter n (- n 1)))

;;test code
(carmichael-test? 561)
(carmichael-test? 1105)
(carmichael-test? 1729)
(carmichael-test? 2465)
(carmichael-test? 2821)
(carmichael-test? 6601)