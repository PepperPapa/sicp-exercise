#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;;完全展开而后规约的求值模型--正则序
;;先求值参数而后应用的方式--应用序

;;我执行该表达式会死循环，说明racket使用的是正则序求值
(test 0 (p))
