#lang sicp

(define (cube x) (* x (* x x)))

;;求和抽象模式
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) b))))

(define (integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (sum-iter k)
      (cond ((= k 0)
             (f a))
            ((= k n)
             (+ (sum-iter (- k 1))
                (f (+ a (* k h)))))
            ((even? k)
             (+ (sum-iter (- k 1))
                (* 2 (f (+ a (* k h))))))
            ((odd? k)
             (+ (sum-iter (- k 1))
                (* 4 (f (+ a (* k h))))))))
    (* (/ h 3)
       (sum-iter n))))
                        
;;test code
(integral cube 0 1 100)
(integral cube 0 1 1000) 