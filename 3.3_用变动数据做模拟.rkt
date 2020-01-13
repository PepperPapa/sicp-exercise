#lang r5rs

;(define x (cons (cons 'a 'b)
 ;               (cons 'c 'd)))
;(define y (cons 'e 'f))
;(set-car! x (cons 'e 'f))
;(set-cdr! x y)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
;(define z (append! x y))
(cdr x) ;; (b c d)

(define w (append! x y))
(cdr x)