#lang racket
(require "math.rkt")

(define (f g)
  (g 2))

;;test cases
(f square)
(f (lambda (z) (* z (+ z 1))))
(f f)
;;(f f)-->(f 2)-->(2 2)-->error 因为2不是过程
;;会输出下面的报错
;; application: not a procedure;
;; expected a procedure that can be applied to arguments
;;  given: 2
;;  arguments...: