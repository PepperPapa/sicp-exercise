#lang racket
(require sicp)

(define (require p)
  (if (not p) (amb)))

;;单词的此类
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;;语法分析
(define (parse-sentence)
    (list 'sentence 
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
    (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
    (require (not (null? *unparsed*)))
    (require (memq (car *unparsed*) (cdr word-list)))
    (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
    (set *unparsed* input)
    (let ((sent (parse-sentence)))
        (require (null? *unparsed*))
        sent))

        
