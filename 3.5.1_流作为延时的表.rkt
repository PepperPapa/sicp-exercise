#lang sicp

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

;; delay必须是特殊过程
;(define (delay exp)
;  (memo-proc (lambda () exp)))

;(define (force delayed-object)
;  (delayed-object))

;;流  cons-stream必须是一个特殊过程 如下定义不能延时求职
;(define (cons-stream a b)
;  (cons a (delay b)))
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

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (begin
        (cons-stream
         (proc (stream-car s))
         (stream-map proc (stream-cdr s))))))
  
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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x)
  (newline))
      
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;;test code
(define a (delay (+ 2 3)))
(force a)

(define s (cons-stream (+ 1 2) (* 9 9)))
(stream-car s)
(stream-cdr s)

(define s1 (stream-enumerate-interval 10 20))
(display-stream s1)
(stream-ref s1 3)
(define s2 (stream-map (lambda(x) (* x x)) s1))
(display-stream s2)

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

;;练习3.51
(display "练习3.51")
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(newline)
(stream-ref x 7)