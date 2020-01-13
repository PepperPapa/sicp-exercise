#lang racket
(require sicp)

;;4.1.1 求值器的内核
;;为避免与基础LISP原生apply过程冲突,实现通过别命的方式保存原生apply
;;(define apply-in-underlying-scheme apply)  ;;这么用会报错

;; eval -- 语法分析
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)  ;;基本表达式
        ((variable? exp) (lookup-variable-value exp env))  ;;变量
        ((quoted? exp) (text-of-quotation exp)) ;;加引号表达式
        ((assignment? exp) (eval-assignment exp env))  ;;变量赋值
        ((definition? exp) (eval-definition exp env))  ;;变量定义
        ((if? exp) (eval-if exp env))  ;;if表达式
        ((lambda? exp)  
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))         ;;lambda表达式
        ((begin? exp)
         (eval-sequence (begin-actions exp) env)) ;;begin表达式
        ((cond? exp) (eval (cond->if exp) env))  ;;cond分情况分析
        ((application? exp)
          ;; changed
         (my_apply (actual-value (operator exp) env)
                (operands exp) env))  ;;组合式，也就是用户定义的过程
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; apply -- 过程执行
(define (my_apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          (list-of-arg-values arguments env)))  ;;changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)  ;;changed
           (procedure-environment procedure))))
        (else
         (error "Unkown procedure type -- APPLY" procedure))))

;;惰性求值
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp obj) (cadr obj))

(define (thunk-env obj) (caddr obj))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value 
                        (thunk-exp obj) 
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)   ;replace exp with its value
           (set-cdr! (cdr obj) '())  ;forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else
         obj)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))


;;过程参数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;条件
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))  ;;changed
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
(define (variable? exp) (symbol? exp))

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


(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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