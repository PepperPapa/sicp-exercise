#lang racket
;;文件名不能包含中文
;;提供double、halve过程
(require "double_halve.rkt")

(define (* a b)
  (define (*-iter a b p)
    (cond ((= b 0) p)
          ((even? b)
           (*-iter (double a) (halve b) p))
          (else
           (*-iter a (- b 1) (+ p a)))))
  (*-iter a b 0))

(* 1 800)
(* 1 801)
(* 2 0)
(* 2 1)
(* 2 10)
(* 2 11)
(* 10 0)
(* 10 1)
(* 10 10)
(* 10 11)