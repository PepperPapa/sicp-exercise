#lang racket
(require rnrs/base-6)

(define (cont-frac n d k)
  (define (iter i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i)
              (iter (+ i 1))))))
  (iter 1))

(define (d i)
  (cond ((= (mod i 3) 2)
         (* 2 (+ (div i 3) 1)))
        (else
         1)))

(define (get-e)
  (+ 2.0
     (cont-frac (lambda (i) 1) d 9)))
;; test cases
(d 1)
(d 2)
(d 3)
(d 4)
(d 5)
(d 6)
(d 7)
(d 8)
(d 9)
(d 10)
(d 11)
(d 12)
(get-e)