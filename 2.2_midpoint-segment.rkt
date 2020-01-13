#lang racket

;; interface for segment
(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (start-segment s)) (x-point(end-segment s))) 2)
  (/ (+ (y-point (start-segment s)) (y-point(end-segment s))) 2)))

;; segment
(define (make-segment ss es)
  (cons ss es))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;; point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

;; test cases
(print-point (make-point 2 3))
(print-point (make-point 0 0))
(print-point (make-point -2 3))
(print-point (midpoint-segment
              (make-segment (make-point 3 0) (make-point 7 20))))