#lang sicp

(define (cube x) (* x (* x x)))

;;求和抽象模式
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  
  (define (term k)
    (cond ((or (= k 0) (= k n))
           (f (+ a (* k h))))
          ((even? k)
           (* 2 (f (+ a (* k h)))))
          ((odd? k)
           (* 4 (f (+ a (* k h)))))))

  (* (/ h 3)
     (sum term 0.0 inc n)))

;;; (define (simpson f a b n)
;;;   (let ((h (/ (- b a) n)))
;;;     (define (sum-iter k)
;;;       (cond ((= k 0)
;;;              (f a))
;;;             ((= k n)
;;;              (+ (sum-iter (- k 1))
;;;                 (f (+ a (* k h)))))
;;;             ((even? k)
;;;              (+ (sum-iter (- k 1))
;;;                 (* 2 (f (+ a (* k h))))))
;;;             ((odd? k)
;;;              (+ (sum-iter (- k 1))
;;;                 (* 4 (f (+ a (* k h))))))))
;;;     (* (/ h 3)
;;;        (sum-iter n))))
                        
;;test code
(simpson cube 0 1 100)
(simpson cube 0 1 1000)