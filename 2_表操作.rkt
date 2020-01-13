#lang racket

;;表相关的操作
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list)
  (define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))
  (iter list '()))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

(define (for-each p lst)
  (cond ((not (null? lst))        
         (p (car lst))
         (for-each p (cdr lst)))))

;; test code
(list-ref (list 1 2 3 4) 0)
(list-ref (list 1 2 3 4) 1)
(list-ref (list 1 2 3 4) 2)
(list-ref (list 1 2 3 4) 3)
(length (list 1 2 3 4 5 6 7 8 9 10))
(append (list) (list 1 2 3 4))
(append (list 1 2 3 4) (list 1 2 3 4))
(reverse (list 1 2 3 46 99 999))

(map (lambda (x) (* x x)) (list -1 -2 3 4 -5 6))
(square-list (list 1 2 3 4))
(for-each (lambda (x)
            (newline)
            (display x))
          (list 1 2 3 4))