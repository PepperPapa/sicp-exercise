#lang racket

;; point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

;; segment
(define (make-segment ss es)
  (cons ss es))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;;矩形表示方式一（依次用矩形连续的4条边来表示矩形）
(define (make-rectangle l1 l2 l3 l4)
  (let ((w l2)
        (h l3))
    (cons w h)))
(define (width rc)
  (abs (- (x-point (start-segment (car rc)))
          (x-point (end-segment (car rc))))))
(define (height rc) 
  (abs (- (y-point (start-segment (cdr rc)))
          (y-point (end-segment (cdr rc))))))

;;矩形表示方式二(对角线上的边可以唯一确定出矩形)
(define (make-rectangle1 l)
  (let ((w (make-segment (start-segment l)
                         (make-point (x-point (end-segment l))
                                     (y-point (start-segment l)))))
        (h (make-segment (make-point (x-point (end-segment l))
                                     (y-point (start-segment l)))
                         (end-segment l))))
    (cons w h)))

;;周长和面积的计算
(define (area rc)
  (* (width rc) (height rc)))
(define (perimeter rc)
  (* 2 (+ (width rc) (height rc))))

(define (print-tectangle-info rc)
  (display "rectangle area is: ")
  (display (area rc))
  (newline)
  (display "rectangle perimeter is: ")
  (display (perimeter rc))
  (newline))

;;test case
;(define rc1 (make-rectangle 9 9))
(define rc1 (make-rectangle (make-segment (make-point 0 9) (make-point 0 0))
                            (make-segment (make-point 0 0) (make-point 9 0))
                            (make-segment (make-point 9 0) (make-point 9 9))
                            (make-segment (make-point 9 9) (make-point 0 9))))
(print-tectangle-info rc1)

(define rc2 (make-rectangle1 (make-segment (make-point 0 0) (make-point 9 9))))
(print-tectangle-info rc2)

(define rc3 (make-rectangle1 (make-segment (make-point 0 9) (make-point 9 0))))
(print-tectangle-info rc3)
