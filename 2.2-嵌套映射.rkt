#lang racket
(require math/number-theory)

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

;;prime-sum?
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;;make-pair-sum
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;;prime-sum-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;;练习2.40
(define (unique-pairs n)
  (accumulate append null
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
(define (prime-sum-pairs-2 n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;;练习2.41
(define (sum-equal-x? seqs)
  (equal? (accumulate + 0 seqs) 12))

(define (unique-three n)
  (filter sum-equal-x?
          (flatmap (lambda (x) x)
                   (map (lambda (p)
                          (map (lambda (k) (list (car p) (cadr p) k))
                               (enumerate-interval 1 (- (cadr p) 1))))
                        (flatmap (lambda (i)
                                   (map (lambda (j) (list i j))
                                        (enumerate-interval 1 (- i 1))))
                                 (enumerate-interval 1 n))))))

;;helper function
(define (show text)
  (display text)
  (display ':)
  (newline))

;; test cases
(show 'prime-sum-pairs)
(prime-sum-pairs 6)
(show 'permutations)
(permutations (list 1 2 3))
(show 'unique-pairs)
(prime-sum-pairs-2 6)
(show 'unique-three)
(unique-three 6)
