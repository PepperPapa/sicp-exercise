#lang racket
;;runtime过程在sicp包中提供,1s=10^6us
(require sicp)

(require "prime.rkt")

(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      (+ n 2)))

;;找到大于n的三个最小的素数
(define (search-for-primes n)
  (define (prime-iter n prime-count)
    (if (< prime-count 3)
        (if (even? n)
            (prime-iter (next-odd n) prime-count)
            (let ((start-time (runtime)))
              (if (prime? n)
                  (begin
                    (newline)
                    (display n)
                    (report-prime (- (runtime) start-time))
                    (prime-iter (next-odd n) (+ prime-count 1)))
                  (prime-iter (next-odd n) prime-count))))))
  (prime-iter n 0))
               

;;test cases
(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)
; (time-prime-test 3)
; (time-prime-test 61)
; (time-prime-test 113)
; (time-prime-test 1049)
; (time-prime-test 10723)
; (time-prime-test 49369)
; (time-prime-test 1234567891)

