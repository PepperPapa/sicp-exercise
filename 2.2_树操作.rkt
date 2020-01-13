#lang racket

;求树叶数量
(define (count-leaves x)
  (cond ((null? x)
         0)
        ((pair? (car x))
         (+ (count-leaves (car x)) (count-leaves (cdr x))))
        (else
         (+ 1 (count-leaves (cdr x))))))

;; test codes
(count-leaves (list 1 (list 2 (list 3 4))))