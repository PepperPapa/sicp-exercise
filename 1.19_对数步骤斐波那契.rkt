#lang sicp

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (display '+)
         (fib-iter a
                  b
                  (+ (square p) (square q))
                  (+ (* 2 (* p q)) (square q))
                  (/ count 2)))
        (else
         (display '+)
         (fib-iter (+ (* b q) (* a (+ p q)))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)