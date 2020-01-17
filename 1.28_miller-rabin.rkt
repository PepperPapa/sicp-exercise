#lang racket
(require "math.rkt")

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
  (if (> n 2)
      (try-it (+ 1 (random (- n 1))))  ;; a > 1 and a < n
      #f))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (if (> n 2)
      (try-it (+ 1 (random (- n 2))))  ;; a > 1 and a < n-1
      #f))

(define (fast-miller-rabin? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n)
         (fast-miller-rabin? n (- times 1)))
        (else
         #f)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else
         #f)))

(fast-miller-rabin? 561 1000)
(fast-prime? 561 10000)

(fast-miller-rabin? 1105 1000)
(fast-prime? 1105 10000)

(fast-miller-rabin? 1729 1000)
(fast-prime? 1729 10000)

(fast-miller-rabin? 2465 1000)
(fast-prime? 2465 10000)

(fast-miller-rabin? 2821 1000)
(fast-prime? 2821 10000)

(fast-miller-rabin? 6601 1000)
(fast-prime? 6601 10000)

(fast-miller-rabin? 3 1000)
(fast-prime? 3 1000)

(fast-miller-rabin? 7 1000)
(fast-prime? 7 1000)

(fast-miller-rabin? 9 1000)
(fast-prime? 9 1000)

(fast-miller-rabin? 11 1000)
(fast-prime? 11 1000)

(fast-miller-rabin? 1 1000)
(fast-prime? 1 1000)

(fast-miller-rabin? 49957 1000)
(fast-prime? 49957 10000)