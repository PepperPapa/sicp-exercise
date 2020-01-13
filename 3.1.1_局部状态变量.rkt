#lang racket
(define (new-withdraw)
  (let ((balance 100))
    (lambda (amount)
      (if (> balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          (error
           "Insufficient Funds")))))

(define B1 (new-withdraw))
;;(B1 10)
;;(B1 200)

(define (make-withdraw balance)
  (lambda (amount)
    (if (> balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        (error
         "Insufficient Funds"))))

;;(define W1 (make-withdraw 100))
;;(define W2 (make-withdraw 100))
;;(W1 50)
;;(W2 70)
;;(W1 40)
;;(W2 40)


(define (make-account balance password)
  (let ((errnum 0))
    (define (withdraw amount)
      (if (> balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          (error
           "Insufficient Funds")))
    (define (deposit amount)
      (begin
        (set! balance (+ balance amount))
        balance))
    (define (call-the-cops)
      (error "too many error password, the cops is coming..."))
    (define (dispatch m pwd)
      (cond ((not (eq? pwd password))
             (set! errnum (+ 1 errnum))
             (if (> errnum 17)
                 (call-the-cops)
                 (error "Incorrect password")))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else
             (error "Unknow request -- MAKE ACCOUNT" m))))
    dispatch))  ;;消息传递

;; test code for make-account
(define acct1 (make-account 100 'qwert))
((acct1 'withdraw 'qwert) 20)
((acct1 'withdraw 'qwert) 20)
((acct1 'deposit 'qwert) 200)
;;((acct1 'withdraw 'qwert3) 200)

;;练习3.1 累加器
(define (make-accumulator initial)
  (lambda (addnum)
    (set! initial (+ initial addnum))
    initial))

(define A (make-accumulator 5))
(A 10)
(A 10)
(A 10)

;;练习3.2
(define (make-monitored f)
  (let ((callnum 0))
    (define (how-many-calls?)
      callnum)
    (define (reset-count)
      (begin
        (set! callnum 0)
        callnum))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else
             (begin
               (set! callnum (+ 1 callnum))
               (f m)))))
    dispatch))

;;test code for make-monitored
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)