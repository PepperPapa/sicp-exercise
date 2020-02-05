#lang racket
(require "math.rkt")

(define (tan-cf x k)
  (define (iter i result)
    (cond ((= i 1)
           (/ x (- 1 result)))
          ((> i 1)           
           (iter (- i 1) (/ (square x)
                            (- (- (* 2 i) 1) result))))))
  (if (= k 1)
      (iter 1 x)
      (iter k (/ (square x)
                 (- (* 2 k) 1)))))

;; test case
(tan-cf (/ (* 5 pi) 12) 10)  ;;75°  
(tan-cf (/ pi 4) 10)  ;;45°
(tan-cf 0 10)  ;;0°