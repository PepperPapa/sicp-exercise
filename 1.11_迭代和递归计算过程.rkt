#lang sicp

;;递归版本
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 30)  ;;;;执行得到结果较慢

;;迭代版本
(define (f-new n)
  (define (f-iter f_n-1 f_n-2 f_n-3 count)
    (if (= count 0)
        f_n-3
        (f-iter (+ f_n-1 (* 2 f_n-2) (* 3 f_n-3)) f_n-1 f_n-2 (- count 1))))
  (f-iter 2 1 0 n))

(f-new 0)
(f-new 1)
(f-new 2)
(f-new 3)
(f-new 4)
(f-new 30)   ;;执行得到结果快很多
