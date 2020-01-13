#lang sicp

;;从records中查找键值为key的记录并返回
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else
         (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)   ;;由于是内部过程，这里不需要传table参数
      (let ((subtable (assoc key-1 (cdr local-table)))) ;;根据key-1确定是哪个子表
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((equal? m 'lookup-proc) lookup)
            ((equal? m 'insert-proc!) insert!)
            (else (error "Unkown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; test code
(put 'letters 'a 97)
(put 'letters 'b 98)
(put 'math '+ 43)
(put 'math '- 45)
(get 'math '+)
(get 'letters 'b)
    
      
