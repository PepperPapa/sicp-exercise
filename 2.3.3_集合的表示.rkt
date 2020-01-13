#lang racket
;;集合作为未排序的表

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        (else
         (element-of-set? x (cdr set)))))

;(element-of-set? 2 (list 4 5 20 8))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;;(adjoin-set 2 (list 3 4 2))

;;交集
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

;(intersection-set (list 2 3 4 6) (list 4 2 8 9))

;;并集
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set (cdr set1) set2)))))
(union-set (list 1 2 3 4) (list 3 4 8 9))
         