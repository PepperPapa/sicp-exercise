#lang sicp

;;队列
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (print-queue queue)
  (let ((front-q (front-ptr queue)))
    (define (show-iter front-q)      
      (if (pair? front-q)
          (begin
            (display (car front-q))
            (display " ")
            (show-iter (cdr front-q)))
          (display "")))
    (show-iter front-q)
    (newline)))

;;队列操作
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)           
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;;基本功能块  门电路
(define (logic-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "unknow signal" s))))

(define (logic-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else
         (error "unknow signal" s1 s2))))

(define (logic-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else
         (error "unknow signal" s1 s2))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logic-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logic-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logic-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;;线路的表示
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))

    (define (accept-action-procedures! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))  ;;为什么要执行？

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedures!)
            (else
             (error "Unkown operations -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null?  procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))

;;待处理表
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  ;;判断time是否应插在首位
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  ;;创建一个新的时间段
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  ;;插入到一个已有的时间段
  (define (add-to-segments! segments)
    (if (= time (segment-time (car segments)))
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))
                                 
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda
                       (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg))))) 
    
;;时间段
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


;;模拟驱动待处理表的运行
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (begin
        (let ((first-item (first-agenda-item the-agenda)))
          (first-item)
          (remove-first-agenda-item! the-agenda)
          (propagate)))))

;;一个简单的实例模拟，在线路上放置一个检测器，只要线路的值发生改变就会打印相关的信息
(define (probe name wire)
  (add-action! wire
              (lambda ()
                (newline)
                (display name)
                (display "  ")
                (display (current-time the-agenda))
                (display "  New-value = ")
                (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(probe 'sum sum)  ;;sum wire增加监控
(probe 'carry carry)  ;;carry wire增加监控
(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)  ;;相当时间开始启动

(set-signal! input-2 1)
(propagate)