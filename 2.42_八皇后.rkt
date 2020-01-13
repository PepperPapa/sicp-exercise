#lang racket
;;accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;enumerate
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;;flatmap
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;;练习2.42 八皇后
(define empty-board '())

(define (safe? k positions)
  (define (noconflict? pk x list)
    (cond ((= x 0 )
           #t)
          ;;pk, (car list)均为行坐标, x为列坐标
          ((or (= pk (car list))   ;;行相同
               (= pk (+ (car list) (- k x)))   ;;对角线,斜率1
               (= pk (- (car list) (- k x))))  ;;对角线,斜率-1
           #f)
          (else
           (noconflict? pk (- x 1) (cdr list)))))
  (noconflict? (car positions) (- k 1) (cdr positions)))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)  (safe? k positions))
         (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)