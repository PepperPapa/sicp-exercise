#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (display-info guess step)
  (display "step: ")
  (display step)
  (display ", guess: ")
  (display guess)
  (newline))

;;test cases
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)  ;;guess < 1会报错
(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x)))) 2.0)