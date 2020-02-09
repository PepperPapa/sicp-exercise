#lang racket

;;helper function
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;有理数的定义
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (and (>= n 0) (< d 0))            
        (cons (- 0 (/ n g)) (abs (/ d g)))
        (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)  
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;;有理数的计算add-rat sub-rat mul-rat div-rat equal-rat?
(define (add-rat x y)
  (make-rat
   (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
   (- (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
   (* (numer x) (denom y))
   (* (denom x) (numer y))))

(define (equal-rat? x y)
  (=
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

;;test case
(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define rat1 (make-rat 3 6))
(define rat2 (make-rat -5 6))
(define rat3 (make-rat 9 -6))
(define rat4 (make-rat -9 -6))

(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(print-rat rat1)
(print-rat rat2)
(print-rat rat3)
(print-rat rat4)