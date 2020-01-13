#lang sicp

;sinx=3sin(x/3)-4(sin(x/3))^3

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sin angle)
  (display '+)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sin (/ angle 3.0)))))

(sin 0)
(sin 1)
(sin 2)
(sin 3)
(sin 4)
(sin 5)
(sin 6)
(sin 12.15)
(sin 600)
(sin 6000)
(sin 60000000)
(sin 600000000000)