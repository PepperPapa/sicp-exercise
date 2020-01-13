#lang racket

;;树的表示
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;(define a (make-tree 2 3 8))
;(entry a)
;(left-branch a)
;(right-branch a)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x null null))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(adjoin-set 10
            (adjoin-set 2
                        (adjoin-set 3
                                    (adjoin-set 40
                                                (adjoin-set 5
                                                            (adjoin-set 60
                                                                        (adjoin-set 7 null)))))))
;;练习2.63、2.64、2.65
                                                            