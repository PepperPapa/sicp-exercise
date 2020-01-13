#lang racket

;;2.3.4Huffman编码树
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? 'leaf (car object)))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))  ;;symbols
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;解码过程
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))
         
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;;2.68
(define (symbol-of-set? x symbols)
  (cond ((null? symbols) false)
        ((eq? x (car symbols)) true)
        (else
         (symbol-of-set? x (cdr symbols)))))

(define (encode-symbol s tree)
  (define (encode x tree result)
    (if (null? tree)
        result
        (let ((left-symbols (symbols (left-branch tree)))
              (right-symbols (symbols (right-branch tree))))
          (cond ((symbol-of-set? x left-symbols)
                 (if (leaf? (left-branch tree))
                     (append result (list 0) )
                     (encode x (left-branch tree) (append result (list 0)))))
                ((symbol-of-set? x right-symbols)
                 (if (leaf? (right-branch tree))
                     (append result (list 1))
                     (encode x (right-branch tree) (append result (list 1)))))))))
  (encode s tree null))
     
;;(encode-symbol 'C sample-tree)
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define sample-symbols '(A D A B B C A))
(encode sample-symbols sample-tree)

;;2.69
(define (successive-merge ordered-sets)
  (cond ((= (length ordered-sets) 0)
         '())
        ((= (length ordered-sets) 1)
         (car ordered-sets))
        (else
         (let ((new-sub-tree (make-code-tree (car ordered-sets)
                                             (cadr ordered-sets)))
               (remained-ordered-sets (cddr ordered-sets)))
           (successive-merge (adjoin-set new-sub-tree remained-ordered-sets))))))
            
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(generate-huffman-tree (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

;;2.70
(define song-tree (generate-huffman-tree (list (list 'A 2)
                             (list 'NA 16)
                             (list 'BOOM 1)
                             (list 'SHA 3)
                             (list 'GET 3)
                             (list 'YIP 9)
                             (list 'JOB 2)
                             (list 'WAH 1))))
(define song-message '(GET A JOB SHA NA NA NA NA NA NA NA NA
                       GET A JOB SHA NA NA NA NA NA NA NA NA
                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                       SHA BOOM))
(define song-code (encode song-message song-tree))
(decode song-code song-tree)
