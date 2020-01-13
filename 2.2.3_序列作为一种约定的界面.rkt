#lang racket

;;filter 
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

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

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;helper function
(define (squares x)
  (* x x))

(define (fib n)
  (cond ((= 0 n) 0)
        ((= 1 n) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;;信号流图
;;求树叶中值为奇数的元素的平方和
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map squares
                   (filter odd? (enumerate-tree tree)))))
;;求0到n生成的斐波那契数列中的偶数组成的序列
(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              null
              (map squares
                   (map fib (enumerate-interval 0 n)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
  
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;练习2.34
;;Horner规则求多项式的值
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (car coefficient-sequence)
                   (* x (horner-eval x (cdr coefficient-sequence)))))
              0
              coefficient-sequence))
;;练习2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;;练习2.37
;;点积
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;;矩阵向量乘积
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))
;;矩阵转秩
(define (transpose mat)
  (accumulate-n cons null mat))
;;矩阵相乘
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
        (map (lambda (row-of-m)
               (matrix-*-vector cols row-of-m)) m)))

;;---test code
(matrix-*-matrix (list (list 1 3 3)
                       (list 1 2 3)
                       (list 1 2 30))
                 (list (list 1 2 3)
                       (list 1 2 3)
                       (list 1 2 3)))
(transpose (list (list 1 2 3 4)
                 (list 1 2 3 4)
                 (list 1 2 3 4)
                 (list 1 2 3 4)))
(dot-product (list 1 2 3 4) (list 4 5 6 6))
(matrix-*-vector (list (list 1 2 3 4)
                       (list 4 5 6 6)
                       (list 6 7 8 9))
                 (list 4 5 6 6))
(accumulate-n cons null (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))
(map abs (list -1 2 -3 4 -5))
(filter odd? (list 1 2 3 4 5))
(accumulate + 0 (list 1 2 3 4 5))
(enumerate-interval 1 10)
(enumerate-interval 2 7)
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))
(even-fibs 10)
(list-fib-squares 10)
(max 1 2 3 4)
(length (list 1 2 3 4))
(append (list 1 2) (list 3 4))
(horner-eval 2 (list 1 3 0 5 0 1))