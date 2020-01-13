#lang racket

;;集合作为排序的表
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (car set) x) true)
        ((< x (car set)) false)
        (else
         (element-of-set? x (cdr set)))))

;;(element-of-set? 0 (list 1 2 3 4))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
       null
      (let ((x1 (car set1))
             (x2 (car set2)))
         (cond ((= x1 x2)
                (cons x1 (intersection-set (cdr set1) (cdr set2))))
               ((< x1 x2)
                (intersection-set (cdr set1) set2))
               ((> x1 x2)
                (intersection-set set1 (cdr set2)))))))

;;(intersection-set (list 1 3 7 9) (list 3 9 10))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;;(adjoin-set 3 (list 1 98))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-set set1 (cdr set2)))))))))


;;(union-set (list 1 2 3) (list 3 4 5))
;;(union-set (list 1 2 3) (list 8 40 50))
;;(union-set (list 10 20 30) (list 4 5))