#lang sicp

;;; (define (* a b)
;;;   (if (= b 0)
;;;       0
;;;       (+ a (* a (- b 1)))))

(define (new-* a b)
  (define (*-iter a b p)
    (cond ((= b 0) p)
          (else
           (display '+)  ;;打印个数表示调用次数
           (*-iter a (- b 1) (+ p a)))))
  (*-iter a b 0))

;;假定double、halve已经存在,注意double过程不能直接使用*过程，否则会死循环
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b)
         (display '+)    ;;打印个数表示调用次数
         (* (double a) (halve b)))
        (else
         (display '+)    ;;打印个数表示调用次数
         (+ a (* (double a) (halve (- b 1)))))))

(* 30 1)
(* 30 2)
(* 30 4)
(* 30 8)
(* 30 16)
(* 30 32)

(new-* 30 1)
(new-* 30 2)
(new-* 30 4)
(new-* 30 8)
(new-* 30 16)
(new-* 30 32)
