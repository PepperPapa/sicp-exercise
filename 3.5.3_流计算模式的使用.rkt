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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x)
  (newline))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;;隐士定义流
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

;;生成流s0,s0+s1,s0+s1+s2,...
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                           (partial-sums s))))

(define t (partial-sums integers))
(stream-ref t 0)
(stream-ref t 1)
(stream-ref t 2)
(stream-ref t 3)
(stream-ref t 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (average a b) (/ (+ a b) 2.0))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)
;;(display-stream (sqrt-stream 2))


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(stream-ref (pi-summands 1) 0)
(stream-ref (pi-summands 1) 1)
(stream-ref (pi-summands 1) 2)
(stream-ref (pi-summands 1) 3)

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-ref pi-stream 0)
(stream-ref pi-stream 1)
(stream-ref pi-stream 2)
(stream-ref pi-stream 3)
(stream-ref pi-stream 4)
(stream-ref pi-stream 5)
(stream-ref pi-stream 6)
(stream-ref pi-stream 7)


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ;s(n-1)
        (s1 (stream-ref s 1))   ;s(n)
        (s2 (stream-ref s 2)))  ;s(n+1)
    (cons-stream (- s2
                    (/ (square (- s2 s1))
                       (+ (- s0 (* 2 s1)) s2)))
                 (euler-transform (stream-cdr s)))))
       
(define (make-tableau transform s)
  (cons-stream s
              (make-tableau transform
                            (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define tau (make-tableau euler-transform pi-stream))

(stream-ref (accelerated-sequence euler-transform pi-stream) 0)
(stream-ref (accelerated-sequence euler-transform pi-stream) 1)
(stream-ref (accelerated-sequence euler-transform pi-stream) 2)
(stream-ref (accelerated-sequence euler-transform pi-stream) 3)
(stream-ref (accelerated-sequence euler-transform pi-stream) 4)
(stream-ref (accelerated-sequence euler-transform pi-stream) 5)
(stream-ref (accelerated-sequence euler-transform pi-stream) 6)
(stream-ref (accelerated-sequence euler-transform pi-stream) 7)
(stream-ref (accelerated-sequence euler-transform pi-stream) 8)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map
     (lambda (x) (list (stream-car s) x))
     (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(define prime-sum-pairs (stream-filter (lambda (pair) (prime? (+ (car pair) (cadr pair))))
               int-pairs))
(display "prime-sum-pairs\n")
(stream-ref prime-sum-pairs 0)
(stream-ref prime-sum-pairs 1)
(stream-ref prime-sum-pairs 2)
(stream-ref prime-sum-pairs 3)
(stream-ref prime-sum-pairs 4)
(stream-ref prime-sum-pairs 5)
(stream-ref prime-sum-pairs 6)




