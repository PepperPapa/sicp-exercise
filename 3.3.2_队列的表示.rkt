#lang sicp

(define (show item)
  (display item)
  (newline))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (print-queue queue)
  (let ((front-q (front-ptr queue)))
    (define (show-iter front-q)      
      (if (pair? front-q)
          (begin
            (display (car front-q))
            (display " ")
            (show-iter (cdr front-q)))
          (display "")))
    (show-iter front-q)
    (newline)))

;;队列操作
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)           
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
  
;; test code
(define q (make-queue))
(print-queue q)
(insert-queue! q 'a)  ; a
(print-queue q)
(insert-queue! q 'b)  ; a b
(print-queue q)
;(insert-queue! q 'c) 
(delete-queue! q)     ; b
(print-queue q)
(delete-queue! q)     ;
(print-queue q)