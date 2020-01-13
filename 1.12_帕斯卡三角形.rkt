#lang sicp

(define (p n)
  (define (p-iter pre_list)
    (cond ((and (= (car pre_list) 1)
                (not (null? (cdr pre_list))))
           (cons 1
                 (cons (+ (car pre_list)
                          (cadr pre_list))
                       (p-iter (cdr pre_list)))))
          ((and (not (null? (car pre_list)))
                (not (null? (cdr pre_list))))
           (cons (+ (car pre_list) (cadr pre_list))
                 (p-iter (cdr pre_list))))
          ((null? (cdr pre_list))
           (cons (car pre_list) '()))))
  (cond ((= n 0) (list 1))
        ((= n 1) (list 1 1))
        ((> n 1)
         (p-iter (p (- n 1))))  
        (else
         (error "ERROR: value n is not valid!"))))

(define (display-blank-iter n)
  (cond ((>= n 0)
         (display " ")
         (display-blank-iter (- n 1)))))

(define (pascal n)
  (define (iter c)
    (cond ((<= c n)
           (display-blank-iter (- n c))
           (display (p c))
           (newline)
           (iter (+ c 1)))))
  (iter 0))

(pascal 10)