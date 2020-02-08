#lang racket
(require rnrs/base-6)

(define (power base n)
  (if (= n 1)
      base
      (* base (power base (- n 1)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    ;(display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (display-info guess step)
  (display "step: ")
  (display step)
  (display ", guess: ")
  (display guess)
  (newline))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (cond ((= n 1)
         f)
        ((> n 1)
         (compose f (repeated f (- n 1))))
        (else
         (error "ERROR: invalid range for n!"))))

(define (nth-root-damp-n-times x n damp-times)
  ;;x的n次方根不动点函数
  (define (root x n)
    (lambda (y)
      (/ x (power y (- n 1)))))
  (fixed-point ((repeated average-damp damp-times) (root x n)) 1.0))

;;x的n次方根,平均阻尼的次数为以2为底方根次数n的对数
(define (nth-root x n)
  (let ((damp-times (div (log n 2) 1)))
    (nth-root-damp-n-times x n damp-times)))

;;test cases
(nth-root-damp-n-times 9 2 1)
(nth-root-damp-n-times 27 3 1)
(nth-root-damp-n-times 81 4 2)
(nth-root-damp-n-times 243 5 2)
(nth-root-damp-n-times 729 6 1)
(nth-root-damp-n-times 2187 7 2)
(nth-root-damp-n-times 6561 8 3)
(nth-root-damp-n-times 19683 9 3)
(nth-root-damp-n-times 59049 10 3)
(nth-root-damp-n-times 177147 11 3)
(nth-root-damp-n-times 531441 12 3)
(nth-root-damp-n-times 1594323 13 3)
(nth-root-damp-n-times 4782969 14 3)
(nth-root-damp-n-times 14348907 15 3)
(nth-root-damp-n-times 43046721 16 4)
(nth-root-damp-n-times 129140163 17 4)
(nth-root-damp-n-times 387420489 18 4)
(nth-root-damp-n-times 1162261467 19 4)
(nth-root-damp-n-times 3486784401 20 4)
(nth-root-damp-n-times 10460353203 21 4)
(nth-root 10460353203 21)