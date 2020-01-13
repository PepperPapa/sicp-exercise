#lang racket

         
;;返回与items第一值有相同奇偶性的值组成的表
(define (same-parity . items)
  (define (iter lst)
    (if (null? lst)
        '()
        (if (= (remainder (car lst) 2) (remainder (car items) 2))
            (cons (car lst) (iter (cdr lst)))
            (iter (cdr lst)))))
  (iter items))

;; test code
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)