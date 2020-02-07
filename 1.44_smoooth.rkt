#lang racket

(define dx 0.00001)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (cond ((= n 1)
         f)
        ((> n 1)
         (compose f (repeated f (- n 1))))
        (else
         (error "ERROR: invalid range for n!"))))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n-times f n)
  (repeated (smooth f) n))

;; test case
((smooth sin) 0)
((smooth sin) 2)
((smooth sin) 4)
((smooth sin) 6)
((smooth sin) 8)

((smooth-n-times sin 20) 0)
((smooth-n-times sin 20) 2)
((smooth-n-times sin 20) 4)
((smooth-n-times sin 20) 6)
((smooth-n-times sin 20) 8)

