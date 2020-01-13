#lang racket
(require sicp)
(require math/number-theory)

;;(define cnt 1)  ;;for debug

;;4.1.1 求值器的内核
;;为避免与基础LISP原生apply过程冲突,实现通过别命的方式保存原生apply
;;(define apply-in-underlying-scheme apply)  ;;这么用会报错

;; eval -- 语法分析与执行分离
(define (eval exp env)
  ((analyze exp) env))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  ;;; ---for debug
  ;;; (newline)
  ;;; (display cnt)
  ;;; (display exp)
  ;;; (set! cnt (+ 1 cnt))
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))  ;;基本表达式
        ((quoted? exp)
         (analyze-quoted exp)) ;;加引号表达式
        ((variable? exp)
         (analyze-variable exp))  ;;变量
        ((assignment? exp)
         (analyze-assignment exp))  ;;变量赋值
        ((definition? exp)
         (analyze-definition exp))  ;;变量定义
        ((if? exp) 
         (analyze-if exp))  ;;if表达式
        ((lambda? exp)  
         (analyze-lambda exp))    ;;lambda表达式
        ((begin? exp)
         (analyze-sequence (begin-actions exp))) ;;begin表达式
        ((cond? exp) 
         (analyze (cond->if exp)))  ;;cond分情况分析
        ((amb? exp)
         (analyze-amb exp))  ;;非确定表达式amb
        ((application? exp)  ;;顺序非常重要，不能和amb的顺序颠倒
          (analyze-application exp))  ;;组合式，也就是用户定义的过程        
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)  ;;基本表示
  (lambda (env succeed fail) 
    (succeed exp fail)))

(define (analyze-quoted exp)          ;;引号表达式
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) 
      (succeed qval fail))))

(define (analyze-variable exp)        ;;变量
  (lambda (env succeed fail) 
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
        (lambda (val fail2)         
          (let ((old-value
                  (lookup-variable-value var env)))
            (set-variable-value! var val env)
            (succeed 'ok 
                     ;; 失败过程
                     (lambda ()
                      (set-variable-value! var old-value env)
                      (fail2)))))
        fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
        (lambda (val fail2)
          (define-variable! var val env)
          (succeed 'ok fail2))
          fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
        ;; success continuation for evaluating the predicate
        ;; to obtain pred-value
        (lambda (pred-value fail2)
          (if (true? pred-value)
            (cproc env succeed fail2)
            (aproc env succeed fail2)))
        fail))))

(define (analyze-lambda exp)        ;;lambda表达式
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail) 
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail) 
      (proc1 env
        ;; success continuation for calling proc1
        (lambda (a-value fail2)
          (proc2 env succeed fail2))
        ;; failure continuation for calling proc1
        fail)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
        (lambda (proc fail2)
          (get-args aprocs
                    env
                    (lambda (args fail3)
                      (execute-application proc args succeed fail3))
                    fail2))
        fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
      ;; success continuation for aproc
      (lambda (arg fail2)
        (get-args (cdr aprocs)
                  env
                  ;; success continuation for this recursive
                  ;; call to get-args
                  (lambda (args fail3)
                    (succeed (cons arg args)
                             fail3))
                  fail2))
      fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unkown procedure type -- EXECUTE-APPLICATION"
          proc))))   
;;amb求值器
(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choice exp) (cdr exp))   ;;('amb choice1 choice2...)

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choice exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                          (try-next (cdr choices))))))
      (try-next cprocs))))

;; apply -- 过程执行
(define (my_apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unkown procedure type -- APPLY" procedure))))


;;过程参数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;条件
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;序列
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;赋值
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;变量定义
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;表4.1.2达式的表示
;;--自求值表达只有数和字符串
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;--变量用符号表示
(define (variable? exp)
  (symbol? exp))

;;--引号表达式的形式是(quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;--赋值的形式是(set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;--定义的形式为(define <var> <value>) 或者
;; (define (<var> <parameter1> .. <parametern>)
;;    <body>)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)    ;; variable
      (caadr exp))) ;; function

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ;; formal parameters
                   (cddr exp))))  ;; body

;;--lambda表达式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;--if条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;--begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
       ((last-exp? seq) (first-exp seq))
       (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;--application
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;--派生表达式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-caluses? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

;; cond 转换为 if 语句
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false            ;; clause else no
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-caluses? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;4.1.3求值器数据结构
;;谓词检测
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;;过程的表示
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;对环境的操作
;;环境表示为一个框架的表
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;环境里的每一个框架都是一对表形成的序对
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;4.1.4做为程序运行这个求值器
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;--已定义的基本过程
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        (list '= =)
        (list 'not not)
        (list 'list list)
        (list 'prime? prime?)
        (list 'member member)
        (list '> >)
        (list '< <)
        (list 'abs abs)
        ;;这里可以继续添加基本过程
        ))

(define (primitive-procedure-names)
  (map car
      primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
      primitive-procedures))

(define (apply-primitive-procedure proc args)
  ;;(apply-in-underlying-scheme
  (apply
    (primitive-implementation proc) args))


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;;ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))


(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                  (procedure-parameters object)
                  (procedure-body object)
                  '<procedure-env>))
    (display object)))

;;初始化全局环境，启动驱动循环
(define the-global-environment (setup-environment))
(driver-loop)

;;; 测试代码，以下代码在驱动循环输入中执行
;;; Amb-Eval input:
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
(define (prime-sum-pair list1 list2)
  (begin 
	(define a (an-element-of list1))
    (define b (an-element-of list2))
    (require (prime? (+ a b)))
    (list a b)))
(prime-sum-pair '(1 3 5 8) '(20 35 110))

;;; 以下为测试输出
;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ok

;;; ;;; Amb-Eval input:

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ok

;;; ;;; Amb-Eval input:

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ok

;;; ;;; Amb-Eval input:

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; (3 20)

;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (3 110)

;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; Amb-Eval value:
;;; (8 35)

;;; ;;; Amb-Eval input:
;;; try-again

;;; ;;; There are no more values of
;;; (prime-sum-pair '(1 3 5 8) '(20 35 110))


;;; let暂未实现，暂有begin代替
;;; (define (prime-sum-pair list1 list2)
;;;   (let ((a (an-element-of list1))
;;;         (b (an-element-of list2)))
;;;     (require (prime? (+ a b)))
;;;     (list a b)))

;;; ;;; 测试代码二
;;; ;;; Amb-Eval input:
;;; (define (multiple-dwelling)
;;;   (begin (define baker (amb 1 2 3 4 5))
;;;         (define cooper (amb 1 2 3 4 5))
;;;         (define fletcher (amb 1 2 3 4 5))
;;;         (define miller (amb 1 2 3 4 5))
;;;         (define smith (amb 1 2 3 4 5))
;;;     (require
;;;       (distinct? (list baker cooper fletcher miller smith)))
;;;     (require (not (= baker 5)))
;;;     (require (not (= cooper 1)))
;;;     (require (not (= fletcher 5)))
;;;     (require (not (= fletcher 1)))
;;;     (require (> miller cooper))
;;;     (require (not (= (abs (- smith fletcher)) 1)))
;;;     (require (not (= (abs (- fletcher cooper)) 1)))
;;;     (list (list 'baker baker)
;;;           (list 'cooper cooper)
;;;           (list 'fletcher fletcher)
;;;           (list 'miller miller)
;;;           (list 'smith smith))))

;;; (define (distinct? items)
;;;   (cond ((null? items) true)
;;;         ((null? (cdr items)) true)
;;;         ((member (car items) (cdr items)) false)
;;;         (else (distinct? (cdr items)))))

;;; (define (require p)
;;;   (if (not p) (amb)))

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ok

;;; ;;; Amb-Eval input:

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ok

;;; ;;; Amb-Eval input:

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ok

;;; ;;; Amb-Eval input:
;;; (multiple-dwelling)

;;; ;;; Starting a new problem 
;;; ;;; Amb-Eval value:
;;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))