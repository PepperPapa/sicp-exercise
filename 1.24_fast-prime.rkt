#lang racket

;;runtime过程在sicp包中提供,1s=10^6us
(require sicp)

(require "prime.rkt")

(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10000)  ;(prime? n)    ;
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(time-prime-test 11)
(time-prime-test 113)
(time-prime-test 1117)
(time-prime-test 11113)
(time-prime-test 1234567891)