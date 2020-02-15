#lang racket

;参考链接 https://www.bbsmax.com/A/8Bz8g23Odx/
;;丘奇计数的基本想法就是通过调用0次函数（这里用f表示）来表示0,通过调用1次函数来表示1,以此类推。
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))
(define (+ a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (f x)
  (display '*))


(define (start-test)
  (display "display 0:")(newline)
  ((zero f) 'a)(newline)
  (display "display 1:")(newline)
  ((one f) 'a)(newline)
  (display "display 2:")(newline)
  ((two f) 'a) (newline)
  (display "display 1+2:")(newline)
  (((+ one two) f) 'a)
  (newline)
  (display "display 0+2+2")(newline)
  (((+ (+ zero two) two) f) 'a)
  (newline)
 
  (display "end.") (newline))

(start-test)