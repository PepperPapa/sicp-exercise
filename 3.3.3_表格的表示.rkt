#lang sicp

;;一维表格的表
(define (make-table) (list '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else
         (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

;;test code
(define t (make-table))
(insert! 'a 1 t)
(insert! 'b 2 t)
(insert! 'c 3 t)
(lookup 'b t)
(lookup 'd t)