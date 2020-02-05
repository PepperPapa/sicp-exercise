#lang racket

(define (cont-frac n d k)
  (define (iter i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i)
              (iter (+ i 1))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (cond ((= 1 i)
           result)
          ((> i 1)
           (iter (- i 1)
                 (/ (n i)
                    (+ (d i)
                       result))))))
  (iter k (/ (n k) (d k))))

(define (get-fi)
  (/ 1.0 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 12)))  ;;k=12

;; test cases
(get-fi)