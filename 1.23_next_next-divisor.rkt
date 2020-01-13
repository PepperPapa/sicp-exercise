#lang racket
(require sicp)

(define (square x) (* x x))

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
         ;(display '+)
         ;(find-divisor n (+ test-divisor 1))
         (find-divisor n (next test-divisor))
         )))

(define (next test-divisor)
  (if (odd? test-divisor)
      (+ test-divisor 2)
      (+ test-divisor 1)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

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

;;test cases
(time-prime-test 1)
(time-prime-test 9)
(time-prime-test 401)
(time-prime-test 1327)
(time-prime-test 3623)
(time-prime-test 11497)
(time-prime-test 49999)
(time-prime-test 1234567891)  ;;3998 2998