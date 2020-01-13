#lang racket
(require math/base)

(define (rand)
  (random-integer 0 999999999999))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;蒙特卡洛方法
(define (monte-caro trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-caro trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))


;;(estimate-pi 100000)

;练习3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (/ (random-integer 0 999999999999) 999999999999))))) ;;这里随机数产生的是整数，导致分布不够随机，最终结果不准确

(define (square x) (* x x))

(define (estimate-integral x1 y1 x2 y2 trials)
  (define (predicate)    
    (let ((x_rand (random-in-range x1 x2))
          (y_rand (random-in-range y1 y2)))
      ;(display (list 'x_rand x_rand))
      ;(newline)
      ;(display (list 'y_rand y_rand))
      ;(newline)
      (cond ((< (square 3)
                (+ (square (- x_rand 5)) (square (- y_rand 7))))
             true)
            ((= (square 3)
                (+ (square (- x_rand 5)) (square (- y_rand 7))))
             true)
            (else false))))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* area (monte-caro trials predicate))))

(estimate-integral 2 4 8 10 100000)   ;;结果不准确，初步排查是random产生的随机数不够随机
  
  