#lang sicp

(define (remainder a b)
  (cond ((< a b) b)
        ((= a b) 0)
        ((< (- a b) b) (- a b))
        (else
         (remainder (- a b) b))))
; n如果是素数,那么需满足:
; n > 1
; n只能被自己整除
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)    ; 如果n=1,没有改条件会陷入死循环
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (divides? a b)
  (= 0 (remainder a b)))

(define (prime? n)
  (= n (smallest-divisor n)))

;;流的定义及方法
;;cons-stream必须是一个特殊过程 如下定义不能延时求职
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define the-empty-stream '())
;;判断流是否为空
(define (stream-null? s)
  (null? s))
;;获取第n个流的值
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       ;;apply就是调用stream-map过程，过程的参数cons组成proc和(map stream-cdr argstreams)
       (apply stream-map 
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (stream-filter proc s)
  (cond ((stream-null? s)
         'done)
        ((proc (stream-car s))
         (cons-stream
          (stream-car s)
          (stream-filter proc (stream-cdr s))))
        (else
         (stream-filter proc (stream-cdr s)))))

;;素数的无穷流
(define (divisible? x y)
  (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve
    (stream-filter
     (lambda (x)
       (not (divisible? x (stream-car stream))))
     (stream-cdr stream)))))

;;隐士定义流
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(stream-ref integers 0)
(stream-ref integers 1)
(stream-ref integers 2)

(define one+one (add-streams ones ones))
(stream-ref one+one 0)
(stream-ref one+one 1)
(stream-ref one+one 20)

;;练习3.53
(define s (cons-stream 1 (add-streams s s)))
(stream-ref s 0)
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)


