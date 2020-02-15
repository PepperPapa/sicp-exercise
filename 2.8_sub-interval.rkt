#lang racket

;;区间对象定义
(define (make-interval a b) (cons a b))
(define (lower-bound r) (car r))
(define (upper-bound r) (cdr r))

;;区间对象的运算
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
              (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- 0 (upper-bound y))
                               (- 0 (lower-bound y)))))

;;test case
(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval 4.465 4.935))
(add-interval r1 r2)
(mul-interval r1 r2)
(div-interval r1 r2)
(sub-interval r1 r2)