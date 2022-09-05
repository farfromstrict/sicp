#lang sicp
(#%require "lib/libs.rkt")
(#%require racket)


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (self-evaluating? exp)
  (cond
    [(number? exp) #t]
    [(string? exp) #t]
    [(boolean? exp) #t]
    [(null? exp) #t]
    [else #f]))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond
    [(null? seq) seq]
    [(last-exp? seq) (first-exp seq)]
    [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))

(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? exps) (null? exps))

(define (first-operand exps) (car exps))

(define (rest-operands exps) (cdr exps))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? 'else (cond-predicate clause)))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ([first (car clauses)] [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'cond->if "ELSE clause isn't last: ~a" clauses))
            (make-if
              (cond-predicate first)
              (sequence->exp (cond-actions first))
              (expand-clauses rest))))))

(define (false? exp) (eq? exp #f))

(define (true? exp) (not (eq? exp #f)))

(define (primitive-procedure? p) (tagged-list? p 'primitive))

(define (primitive-implementation p) (cadr p))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (make-procedure0 parameters body env)
  (list 'procedure parameters body env))

(define make-procedure make-procedure0)

(define (compound-procedure? p) (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (mcdr env))

(define (first-frame env) (mcar env))

(define (make-frame variables values) (mcons variables values))

(define (frame-variables frame) (mcar frame))

(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (let ([vars-len (length vars)] [vals-len (length vals)])
    (cond
      [(< vars-len vals-len) (error 'extend-env "Too many arguments supplied: ~a, ~a" vars vals)]
      [(> vars-len vals-len) (error 'extend-env "Too few arguments supplied: ~a, ~a" vars vals)]
      [else (mcons (make-frame (list->mlist vars) (list->mlist vals)) base-env)])))

(define (scan var frame found-proc end-proc)
  (let loop ([vars (frame-variables frame)] [vals (frame-values frame)])
    (cond
      [(null? vars) (end-proc)]
      [(eq? var (mcar vars)) (found-proc (cons vars vals))]
      [else (loop (mcdr vars) (mcdr vals))])))

(define (scan-frame var frame) (scan var frame (lambda (rst) (cons (mcar (car rst)) (mcar (cdr rst)))) (const #f)))

(define (scan-first-frame var env) (if (null? env) #f (scan-frame var (first-frame env))))

(define (lookup-variable-exp var env)
  (cond
    [(null? env) (error 'lookup-variable-exp "Unbound variable: ~a" var)]
    [else
     (scan var (first-frame env)
       (lambda (rst)
         (let ([val (mcar (cdr rst))])
           (if (eq? val '*unassigned*)
               (error 'lookup-variable-exp "Variable is unassigned: ~a" var)
               val)))
       (lambda () (lookup-variable-exp var (enclosing-environment env))))]))

(define (set-variable-value! var val env)
  (cond
    [(null? env) (error 'set-variable-value! "Unbound variable: ~a" var)]
    [else
     (scan var (first-frame env)
       (lambda (rst) (set-car! (cdr rst) val))
       (lambda () (set-variable-value! var val (enclosing-environment env))))]))

(define (define-variable! var val env)
  (scan var (first-frame env)
    (lambda (rst) (set-car! (cdr rst) val))
    (lambda () (add-binding-to-frame! var val (first-frame env)))))

(define (mapply procedure arguments env)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments)]
    [(compound-procedure? procedure)
     (eval-sequence
       (procedure-body procedure)
       (extend-environment
        (procedure-parameters procedure)
        arguments
        (procedure-environment procedure)))]
    [else (error 'mapply "Unknown procedure type: ~a" procedure)]))

(define (make-env . lsts)
  (extend-environment
    (map car lsts)
    (map (lambda (p) (list 'primitive (cadr p))) lsts)
    '()))

(define (make-rules)
  (define (make-pair k f) (mcons k f))
  (define (pair-key p) (mcar p))
  (define (pair-proc p) (mcdr p))
  (define rules
    (list
      (mcons '*pred*
        (mlist
          (make-pair self-evaluating? (lambda (exp env) exp))
          (make-pair variable? lookup-variable-exp)))
      (mcons '*pred* '())
      (mcons '*tag* '())
      (mcons '*tag*
        (mlist
          (make-pair 'quote (lambda (exp env) (text-of-quotation exp)))
          (make-pair 'set! eval-assignment)
          (make-pair 'define eval-definition)
          (make-pair 'if eval-if)
          (make-pair 'lambda (lambda (exp env)
                               (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env)))
          (make-pair 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
          (make-pair 'cond (lambda (exp env) (eval (cond->if exp) env)))))
      (mcons '*pred* '())
      (mcons '*pred*
       (mlist
         (make-pair application? (lambda (exp env)
                                   (mapply (eval (operator exp) env)
                                           (list-of-values (operands exp) env)
                                           env)))))))
  (define sub-rules-1 '())
  (define sub-rules-2 '())
  (define sub-rules-4 '())
  (define (get-sub-rules idx)
    (let loop ([rules rules] [idx idx])
      (cond
        [(null? rules) #f]
        [(= idx 0) (mcdr (car rules))]
        [else (loop (cdr rules) (- idx 1))])))
  (define (set-sub-rules! idx lst)
    (let loop ([rules rules] [idx idx])
      (cond
        [(null? rules) #f]
        [(= idx 0) (set-cdr! (car rules) lst)]
        [else (loop (cdr rules) (- idx 1))])))
  (define (reset)
    (set-sub-rules! 1 '())
    (set-sub-rules! 2 '())
    (set-sub-rules! 4 '()))
  (define (save)
    (set! sub-rules-1 (get-sub-rules 1))
    (set! sub-rules-2 (get-sub-rules 2))
    (set! sub-rules-4 (get-sub-rules 4)))
  (define (load)
    (set-sub-rules! 1 sub-rules-1)
    (set-sub-rules! 2 sub-rules-2)
    (set-sub-rules! 4 sub-rules-4))
  (define (assoc key lst match?)
    (cond
      [(null? lst) #f]
      [(match? key (mcar lst)) (mcar lst)]
      [else (assoc key (mcdr lst) match?)]))
  (define (assoc-exp-tag exp lst)
    (assoc (car exp) lst (lambda (k e) (eq? k (pair-key e)))))
  (define (assoc-exp-pred exp lst)
    (assoc exp lst (lambda (k e) ((pair-key e) k))))
  (define (get exp)
    (let loop ([rules rules])
      (cond
        [(null? rules) (error 'make-rules "Unknown expression type: ~a" exp)]
        [else
         (let ([sub-rules (car rules)])
           (let ([type (mcar sub-rules)] [lst (mcdr sub-rules)])
             (let ([assoc-method (if (eq? type '*tag*) assoc-exp-tag assoc-exp-pred)])
               (let ([record (assoc-method exp lst)])
                 (cond
                   [record (pair-proc record)]
                   [else (loop (cdr rules))])))))])))
  (define (put! key val idx)
    (let loop ([rules rules] [idx idx])
      (cond
        [(null? rules) #f]
        [(= idx 0)
         (let ([sub-rules (car rules)])
           (let ([record (assoc key (mcdr sub-rules) (lambda (k e) (eq? k (pair-key e))))])
             (cond
               [record (set-cdr! record val)]
               [else (set-cdr! sub-rules (mcons (make-pair key val) (mcdr sub-rules)))])))]
        [else (loop (cdr rules) (- idx 1))])))
  (define (del! key idx)
    (let loop ([rules rules] [idx idx])
      (cond
        [(null? rules) #f]
        [(= idx 0)
         (let sub-loop ([lst (car rules)])
           (cond
             [(null? (mcdr lst)) #f]
             [(eq? key (pair-key (mcar (mcdr lst))))
              (set-cdr! lst (mcdr (mcdr lst)))]
             [else (sub-loop (mcdr lst))]))]
        [else (loop (cdr rules) (- idx 1))])))
  (define (dispatch m)
    (cond
      [(eq? m 'reset) (reset)]
      [(eq? m 'save) (save)]
      [(eq? m 'load) (load)]
      [(eq? m 'get) get]
      [(eq? m 'putt!) (lambda (tag proc) (put! tag proc 2))]
      [(eq? m 'put0!) (lambda (pred? proc) (put! pred? proc 1))]
      [(eq? m 'put1!) (lambda (pred? proc) (put! pred? proc 4))]
      [(eq? m 'del!)
       (lambda (key)
         (cond
           [(symbol? key) (del! key 2)]
           [else (del! key 1) (del! key 4)]))]
      [(eq? m 'print) (printf "~a\n" rules)]
      [else (error 'make-rules "Unknown message: ~a" m)]))
  dispatch)

(define global-rules (make-rules))

(define (eval exp env)
;   (printf "+++ [  ] ~a\n" exp)
  (((global-rules 'get) exp) exp env))


;;; ex 4.1
(ex 1
(define records '())

(define (eval exp env)
  (let ([op (car exp)] [a (cadr exp)] [b (caddr exp)])
    (set! records (append records (list op)))
    (cond
      [(eq? op '+) (+ a b)]
      [(eq? op '-) (- a b)]
      [(eq? op '*) (* a b)])))

(define (list-of-values/l2r exps env)
  (if (no-operands? exps)
      '()
      (let ([first (eval (first-operand exps) env)])
        (let ([rest (list-of-values/l2r (rest-operands exps) env)])
          (cons first rest)))))

(define (list-of-values/r2l exps env)
  (if (no-operands? exps)
      '()
      (let ([rest (list-of-values/r2l (rest-operands exps) env)])
        (let ([first (eval (first-operand exps) env)])
          (cons first rest)))))

(define exps '((+ 1 2) (- 10 5) (* 2 3)))

(set! records '())
(?== '(3 5 6) (list-of-values/l2r exps '()))
(?== '(+ - *) records)

(set! records '())
(?== '(3 5 6) (list-of-values/r2l exps '()))
(?== '(* - +) records)
)


;;; ex 4.2
(ex 2
;; (define (application? exp) (pair? exp))
;; define 和 set! 语句都会被判断为 application, 不会进入 assignment? 和 definition? 分支
;
; (define (eval exp env)
;   (cond
;     [...]
;     [(application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env))]
;     [(assignment? exp) (eval-assignment exp env)]
;     [(definition? exp) (eval-definition exp env)]
;     [else (error 'eval "Unknown expression type: ~a" exp)]))
;
; (define (application? exp) (tagged-list? exp 'call))
;
; (define x 3)
; (define y 4)
; (set! x 4)
; (call + x y)
)


;;; ex 4.3
(ex 3
(define rules '())

(define (assoc type lst)
  (cond
    [(null? lst) #f]
    [(eq? type (mcar (car lst))) (car lst)]
    [else (assoc type (cdr lst))]))

(define (get type)
  (let ([record (assoc type rules)])
    (if record
        (mcdr record)
        #f)))

(define (put type proc)
  (let ([record (assoc type rules)])
    (if record
        (set-cdr! record proc)
        (set! rules (cons (mcons type proc) rules)))))

(define (eval/ddd exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [(get (exp-type exp)) ((get (exp-type exp)) exp env)]
    [(application? exp)
     (apply (eval/ddd (operator exp) env)
            (list-of-values (operands exp) env))]
    [else (error 'eval/ddd "Unknown expression type: ~a" exp)]))

(define (exp-type exp) (car exp))

(define (lookup-variable-value exp env) #f)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (install)
  (set! rules '())
  (put 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'set! eval-assignment)
  (put 'define eval-definition)
  (put 'if eval-if)
  (put 'lambda
    (lambda (exp env)
      (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'begin
    (lambda (exp env)
      (eval-sequence (begin-actions exp) env)))
  (put 'cond
    (lambda (exp env)
      (eval/ddd (cond->if exp) env)))
  'ok)

(@update ([eval eval/ddd])
  (install)
  (?== 2 (eval/ddd '(if #f 1 2) '())))
)


;;; ex 4.4
(ex 4
(define (eval-and exp env)
  (let ([clauses (cdr exp)])
    (cond
      [(null? clauses) #t]
      [else
       (let loop ([clauses clauses])
         (let ([first (eval (car clauses) env)])
           (cond
             [(false? first) #f]
             [(null? (cdr clauses)) first]
             [else (loop (cdr clauses))])))])))

(define (eval-or exp env)
  (let ([clauses (cdr exp)])
    (let loop ([clauses clauses])
      (cond
        [(null? clauses) #f]
        [(true? (eval (car clauses) env)) #t]
        [else (loop (cdr clauses))]))))

(define (verify)
  (?true (eval '(and) '()))
  (?false (eval '(and 1 #f 2) '()))
  (?== 2 (eval '(and 1 #t (if #t 2 1)) '()))
  (?false (eval '(or) '()))
  (?true (eval '(or #f (if #t 2 1)) '()))
  (?false (eval '(or #f #f) '()))
  (void))

(define (and->if exp)
  (let ([clauses (cdr exp)])
    (cond
      [(null? clauses) #t]
      [(null? (cdr clauses)) (car clauses)]
      [else
       (make-if
         (car clauses)
         (cons 'and (cdr clauses))
         #f)])))

(define (or->if exp)
  (let ([clauses (cdr exp)])
    (cond
      [(null? clauses) #f]
      [else
       (make-if
         (car clauses)
         #t
         (cons 'or (cdr clauses)))])))

((global-rules 'putt!) 'and eval-and)
((global-rules 'putt!) 'or eval-or)
------
(verify)
(@>> "----------")
((global-rules 'putt!) 'and (lambda (exp env) (eval (and->if exp) env)))
((global-rules 'putt!) 'or (lambda (exp env) (eval (or->if exp) env)))
(verify)
)


;;; ex 4.5
(ex 5
(define (eval-cond exp env)
  (let loop ([clauses (cdr exp)])
    (cond
      [(null? clauses) #f]
      [else
       (let ([first (car clauses)] [rest (cdr clauses)])
         (cond
           [(eq? 'else (car first))
            (cond
              [(null? rest) (eval (sequence->exp (cdr first)) env)]
              [else (error 'eval-cond "ELSE clause is not last: ~a" exp)])]
           [(null? (cdr first)) (error 'eval-cond "clause has no actions: ~a" first)]
           [(eq? '=> (cadr first))
            (cond
              [(null? (cddr first)) (error 'eval-cond "clause has no recipient: ~a" first)]
              [else
               (let ([test-result (eval (car first) env)] [recipient (caddr first)])
                 (cond
                   [(true? test-result) (mapply (eval recipient env) (cons test-result '()) env)]
                   [else (loop (cdr clauses))]))])]
           [else
            (let ([predicate (eval (car first) env)] [actions (cdr first)])
              (cond
                [(true? predicate) (eval (sequence->exp actions) env)]
                [else (loop (cdr clauses))]))]))])))

((global-rules 'putt!) 'cond eval-cond)

(define genv (make-env `(inc ,inc)))

(define s
  '(cond
     [#f 10 (if #t 1 2)]
     [10 => inc]
     [else 100]))
(?== 11 (eval s genv))

;((global-rules 'del!) 'cond)
)


;;; ex 4.6
(ex 6
(define (let->combination exp)
  (let ([param-pairs (cadr exp)] [body (cddr exp)])
    (let ([params (map car param-pairs)] [args (map cadr param-pairs)])
      (cons (make-lambda params body) args))))

((global-rules 'putt!) 'let (lambda (exp env) (eval (let->combination exp) env)))
------
(define genv (make-env `(+ ,+) `(* ,*)))

(define s
  '(let ([x 10] [y 20])
     (+ x y)
     (* x y)))
(?== 200 (eval s genv))
)


;;; ex 4.7
(ex 7
(define (let*->nested-lets exp)
  (let ([param-pairs (cadr exp)] [body (cddr exp)])
    (let loop ([pairs param-pairs])
      (let ([first (car pairs)] [rest (cdr pairs)])
        (cond
          [(null? rest) (cons 'let (cons (list first) body))]
          [else (list 'let (list first) (loop rest))])))))

((global-rules 'putt!) 'let* (lambda (exp env) (eval (let*->nested-lets exp) env)))
------
(define genv (make-env `(+ ,+) `(* ,*)))

(define s
  '(let* ([x 3] [y (+ x 2)] [z (+ x y 5)])
     (* x z)))
(?== 39 (eval s genv))
)


;;; ex 4.8
(ex 8
(define (let->combination/v2 exp)
  (cond
    [(symbol? (cadr exp))
     (let ([id (cadr exp)] [param-pairs (caddr exp)] [body (cdddr exp)])
       (let ([params (map car param-pairs)] [args (map cadr param-pairs)])
         (cons
           (make-lambda
             params
             (list (list 'define id (make-lambda params body)) (cons id params)))
           args)))]
    [else (let->combination exp)]))

((global-rules 'putt!) 'let (lambda (exp env) (eval (let->combination/v2 exp) env)))
------
(define genv (make-env `(+ ,+) `(- ,-) `(= ,=)))

(define s
  '(let fib-iter ([a 1] [b 0] [count 10])
     (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1)))))
(?== 55 (eval s genv))
)


;;; ex 4.9
(ex 9
; (do ([id init-expr step-expr-maybe] ...)
;     (stop?-expr finish-expr ...)
;   expr ...)
; --> (使用匿名lambda避免命名冲突)
; ((lambda ()
;    (define loop
;      (lambda (id ...)
;        (cond
;          [stop?-expr finish-expr ...]
;          [else
;           expr ...
;           (loop step-expr ...)])))
;    (loop init-expr ...)))

(define (do->combination exp)
  (let ([params (cadr exp)] [stop-clause (caddr exp)] [body-exprs (cdddr exp)])
    (let ([ids (map car params)]
          [init-exprs (map cadr params)]
          [step-exprs (map (lambda (x) (if (null? (cddr x)) (car x) (caddr x))) params)])
      (cons
        (make-lambda
          '()
          (list
            (list
              'define
              'loop
              (make-lambda
                ids
                (list
                  (list
                    'cond
                    stop-clause
                    (append (list 'else) body-exprs (list (cons 'loop step-exprs)))))))
            (cons 'loop init-exprs)))
        '()))))

; (for ([id seq-expr] ...) body ...)
; -->
; ((lambda ()
;    (define (helper id ...) body ...)
;    (define (loop id ...)
;      (cond
;        [(null? id) (void)]
;        [else
;         (helper (car id) ...)
;         (loop (cdr id) ...)]))
;    (loop seq-expr ...)))

(define (for->combination exp)
  (let ([params (cadr exp)] [body (cddr exp)])
    (let ([ids (map car params)] [seq-exprs (map cadr params)])
      (list (make-lambda
              '()
              (list
                (cons 'define (cons (cons 'helper ids) body))
                (list
                  'define
                  (cons 'loop ids)
                  (list
                    'cond
                    (list (list 'null? (car ids)) (list 'void))
                    (list
                      'else
                      (cons 'helper (map (lambda (id) (list 'car id)) ids))
                      (cons 'loop (map (lambda (id) (list 'cdr id)) ids)))))
                (cons 'loop seq-exprs)))))))


((global-rules 'putt!) 'do (lambda (exp env) (eval (do->combination exp) env)))
((global-rules 'putt!) 'for (lambda (exp env) (eval (for->combination exp) env)))
------
(define genv (make-env `(+ ,+) `(* ,*) `(> ,>) `(null? ,null?) `(cons ,cons) `(car ,car) `(cdr ,cdr) `(void ,void)))

(define s1
  '(do ([x 1 (+ x 2)] [y 10])
       ((> x y) (set! a (* a x)))
     (set! a (+ a x))))

(eval '(define a 0) genv)
(eval s1 genv)
(?== 275 (cdr (scan-first-frame 'a genv)))

(define s2
  '(for ([i '(1 2 3)] [j '(10 20 30)])
     (define p (* i j))
     (set! a (+ a p))))
(eval '(set! a 0) genv)
(eval s2 genv)
(?== 140 (cdr (scan-first-frame 'a genv)))
)


;;; ex 4.10
(ex 10
(global-rules 'save)
(global-rules 'reset)

;; (let ID = EXPR) ===> (define ID EXPR)
(define (let->combination exp)
  (let ([id (cadr exp)] [expr (cadddr exp)])
    (list 'define id expr)))

;; (fn ID (PARAMS ...) { BODY ... }) ===> (define ID (lambda (PARAMS ...) BODY ...))
(define (fn->combination exp)
  (let ([id (cadr exp)] [params (caddr exp)] [body (cadddr exp)])
    (list 'define id (make-lambda params body))))

;; (PRED ? CONSEQ : ALTER) ===> (if PRED CONSEQ ALTER)
(define (ternary? exp)
  (and (pair? exp) (not (null? (cdr exp))) (eq? '? (cadr exp))))

(define (ternary->combination exp)
  (let ([predicate (car exp)] [consequent (caddr exp)] [alternative (car (cddddr exp))])
    (list 'if predicate consequent alternative)))

((global-rules 'putt!) 'let (lambda (exp env) (eval (let->combination exp) env)))
((global-rules 'putt!) 'fn (lambda (exp env) (eval (fn->combination exp) env)))
((global-rules 'put0!) ternary? (lambda (exp env) (eval (ternary->combination exp) env)))

(define fns
  '(fn abs (a b) {
    (let c = (- a b))
    ((> c 0) ? c : (- c))
  }))

(define genv (make-env `(- ,-) `(> ,>)))

(eval fns genv)
(eval '(let a = 20) genv)
(eval '(let b = 30) genv)
(eval '(let c = (abs a b)) genv)
(?== 10 (cdr (scan-first-frame 'c genv)))

(global-rules 'reset)
(global-rules 'load)
)


;;; 4.11
(ex 11
(define (make-pair var val) (mcons var val))

(define (pair-var p) (mcar p))

(define (pair-val p) (mcdr p))

(define (make-frame vars vals)
  (mcons
    '*frame*
    (let loop ([vars vars] [vals vals])
      (cond
        [(null? vars) '()]
        [else (mcons (make-pair (mcar vars) (mcar vals)) (loop (mcdr vars) (mcdr vals)))]))))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (mcons (make-pair var val) (mcdr frame))))

(define (extend-environment vars vals base-env)
  (let ([vars-len (length vars)] [vals-len (length vals)])
    (cond
      [(< vars-len vals-len) (error 'extend-env "Too many arguments supplied: ~a, ~a" vars vals)]
      [(> vars-len vals-len) (error 'extend-env "Too few arguments supplied: ~a, ~a" vars vals)]
      [else (mcons (make-frame (list->mlist vars) (list->mlist vals)) base-env)])))

(define (scan var frame found-proc end-proc)
  (let loop ([pairs (mcdr frame)])
    (cond
      [(null? pairs) (end-proc)]
      [(eq? var (pair-var (mcar pairs))) (found-proc (mcar pairs))]
      [else (loop (mcdr pairs))])))

(define (lookup-variable-exp var env)
  (cond
    [(null? env) (error 'lookup-variable-exp "Unbound variable: ~a" var)]
    [else
     (scan var (first-frame env)
       (lambda (pair) (pair-val pair))
       (lambda () (lookup-variable-exp var (enclosing-environment env))))]))

(define (set-variable-value! var val env)
  (cond
    [(null? env) (error 'set-variable-value! "Unbound variable: ~a" var)]
    [else
     (scan var (first-frame env)
       (lambda (pair) (set-cdr! pair val))
       (lambda () (set-variable-value! var val (enclosing-environment env))))]))

(define (define-variable! var val env)
  (scan var (first-frame env)
    (lambda (pair) (set-cdr! pair val))
    (lambda () (add-binding-to-frame! var val (first-frame env)))))

(define (make-env . lsts)
  (extend-environment (map car lsts) (map cadr lsts) '()))

(define genv (make-env '(a 10) '(b 20) `(+ ,+)))
(?== 10 (lookup-variable-exp 'a genv))
(?== 20 (lookup-variable-exp 'b genv))
(set-variable-value! 'a 100 genv)
(?== 100 (lookup-variable-exp 'a genv))
(define-variable! 'c 30 genv)
(?== 30 (lookup-variable-exp 'c genv))
)


;;; ex 4.12
(ex 12
;; 上述代码实现中已包含
)


;;; ex 4.13
(ex 13
;; 仅在当前frame进行解绑定
(define (make-unbound! var frame)
  (let ([vars (frame-variables frame)] [vals (frame-values frame)])
    (cond
      [(null? vars) (void)]
      [(eq? var (mcar vars))
       (set-car! frame (mcdr vars))
       (set-cdr! frame (mcdr vals))]
      [else
       (let loop ([vars vars] [vals vals])
         (cond
           [(null? (mcdr vars)) (void)]
           [(eq? var (mcar (mcdr vars)))
            (set-cdr! vars (mcdr (mcdr vars)))
            (set-cdr! vals (mcdr (mcdr vals)))]
           [else (loop (mcdr vars) (mcdr vals))]))])))

(define (eval-unbinding var env) (make-unbound! var (first-frame env)))

((global-rules 'putt!) 'unbind (lambda (exp env) (eval-unbinding (cadr exp) env)))

(define genv (make-env `(+ ,+)))

(?false (scan-first-frame 'a genv))
(eval '(define a 10) genv)
(?== '(a . 10) (scan-first-frame 'a genv))
(eval '(unbind a) genv)
(?false (scan-first-frame 'a genv))
(eval '(define a 100) genv)
(eval '(define b 200) genv)
(?== '(a . 100) (scan-first-frame 'a genv))
(eval '(unbind a) genv)
(?false (scan-first-frame 'a genv))
)


;;; ex 4.14
(ex 14
;; 在本求值器中, procedure 实际上都被构造成 ('primitive #<procedure>) ('procedure parameters body env) 的形式, 无法被系统的 map 方法识别

(define s
  '(define (map f lst)
     (cond
       [(null? lst) '()]
       [else (cons (f (car lst)) (map f (cdr lst)))])))

(define genv (make-env `(null? ,null?) `(cons ,cons) `(car ,car) `(cdr ,cdr) `(- ,-)))

(eval s genv)
(?== '(-1 -2 -3) (eval '(map - '(1 2 3)) genv))
)


;;; ex 4.15
(ex 15
; (define (run-forever) (run-forever))

; (define (try p)
;   (if (halts? p p)
;       (run-forever)
;       'halted))

; (try try)
;; 如果过程 try 对 对象try 终止, 则 (halts? try try) 返回 #t, 将对象 try 代入 (try p) 的参数后, (try try) 无限循环, 矛盾
;; 反之, 如果过程try 对 对象try 不终止, 则 (halts? try try) 返回 #f, (try try) 返回 'halted, 一样矛盾
)


;;; ex 4.16
(ex 16
;; a) 见上述函数实现

(define (scan-out-defines body)
  (let loop ([exps body] [def-exps '()] [no-def-exps '()])
    (cond
      [(null? exps)
       (cond
         [(null? def-exps) body]
         [else
          (cons
            (cons
              'let
              (cons
                (map (lambda (exp) (cons (cadr exp) '('*unassigned*))) def-exps)
                (append (map (lambda (exp) (cons 'set! (cdr exp))) def-exps) no-def-exps)))
            '())])]
      [(tagged-list? (car exps) 'define) (loop (cdr exps) (append def-exps (list (car exps))) no-def-exps)]
      [else (loop (cdr exps) def-exps (append no-def-exps (list (car exps))))])))

(define (make-procedure/v2 parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(@update! ([make-procedure make-procedure/v2]))

(define body-exprs
  '((define a 10)
    (define b 20)
    (set! a (+ b c))
    (define c 30)
    a))

(define genv (make-env `(+ ,+)))
(?== '() (eval '(let () ()) genv))
(?== 30 (eval '(let () (+ 10 20)) genv))
(?== 50 (eval (append (list 'let '()) body-exprs) genv))

;; c) 安装在 make-procedure 过程中更好, make-procedure 是求值过程, procedure-body 是应用过程
;;    一般而言, 求值过程只有一次, 而应用过程可能会有多次, 每次应用过程中都调用 scan-out-defines 过程会耗费资源
)


;;; ex 4.17
(ex 17
;; 变换:
;; (lambda (...) body ...)
;; ==> (lambda (...) (let ([...] ...) body ...))
;; ==> (lambda (...) ((lambda (...) body ...) ...))
;;
;; 引入let语句, 等效于lambda, 所以会多一个框架
;;
;; 变换前:
;; [E1: {vars...} {u => e1} {v => e2}] `e3`
;;
;; 变换后:
;; [E1: {vars...}]
;; [E2: {u => 'unassigned} {v => 'unassigned}] `(set! u e1) ...`
;;
;; 不使用let语句即可, 效果与使用let语句一致
;; (lambda (vars)
;;   (define u 'unassigned)
;;   (define v 'unassigned)
;;   (set! u <e1>)
;;   (set! v <e2>)
;;   <e3>)
;;
;; 也可以使用惰性求值, 可以获得更广泛的作用域同时性
;; (lambda (vars)
;;   (define u (delay <e1>))
;;   (define v (delay <e2>))
;;   <e3>)
;;
;; 例如:
;; (lambda ()
;;   (define a 10)
;;   (define c (+ a b))
;;   (define b 20)
;;   c)
;; 使用普通求值方式时, 不管是否使用let语句, 都会报错; 使用惰性求值时可以获得预期值
)


;;; ex 4.18
(ex 18
;; 正文中的方式可以工作, 练习中的不行
;; 练习中的方式, 在求值 [a (integral (delay dy) y0 dt)] 时因为有delay的存在可以正常通过
;; 但是当求值 [b (stream-map f y)] 时, y 的值为 '*unassigned*, 无法正常运行
;; 正文中的方式, 此时y已经被赋值为stream, 可以正常运行
)


;;; ex 4.19
(ex 19
;; 相比 Ben 的观点, Eva 的观点更符合函数式的原则
;; racket 系统和 SICP 正文中的变换方式都是按 Alyssa 的观点实现, 会报错 a: undefined;
;; 使用惰性求值可实现 Eva 的预期
;; (lambda (vars...)
;;   (define u <e1>)
;;   (define v <e2>)
;;   <e3>)
;; ==>
;; (lambda (vars...)
;;   (define u (delay <e1>))
;;   (define v (delay <e2>))
;;   <e3>)

(define (scan-out-defines/lazy body)
  (let loop ([exprs body] [defs '()] [no-defs '()])
    (cond
      [(null? exprs)
       (cond
         [(or (null? defs) (null? no-defs)) body]
         [else
          (append
            (map (lambda (exp) (cons 'define/lazy (cdr exp))) defs)
            no-defs)])]
      [else
       (let ([first (car exprs)] [rest (cdr exprs)])
         (cond
           [(tagged-list? first 'define)
            (loop rest (append defs (list first)) no-defs)]
           [else (loop rest defs (append no-defs (list first)))]))])))

(define (make-lazy val) (cons '*lazy* val))

(define (lazy? val) (and (pair? val) (eq? '*lazy* (car val))))

(define (get-value/lazy val env) (if (lazy? val) (eval (cdr val) env) val))

(define (eval-definition/lazy exp env)
  (cond
    [(pair? (cadr exp)) (eval-definition exp env)]
    [(tagged-list? (caddr exp) 'lambda) (eval-definition exp env)]
    [else
     (let ([var (cadr exp)] [val (caddr exp)])
       (define-variable! var (make-lazy val) env))]))

(define (make-procedure/lazy parameters body env)
  (let ([new-body (scan-out-defines/lazy body)])
    (list 'procedure parameters new-body env)))

(define (lookup-variable-exp/lazy var env)
  (cond
    [(null? env) (error 'lookup-variable-exp "Unbound variable: ~a" var)]
    [else
     (scan var (first-frame env)
       (lambda (rst) (get-value/lazy (mcar (cdr rst)) env))
       (lambda () (lookup-variable-exp var (enclosing-environment env))))]))

(define (mapply/lazy procedure arguments env)
  (let ([new-args (map (lambda (arg) (get-value/lazy arg env)) arguments)])
    (cond
      [(primitive-procedure? procedure)
       (apply-primitive-procedure procedure new-args)]
      [(compound-procedure? procedure)
       (eval-sequence
         (procedure-body procedure)
         (extend-environment
          (procedure-parameters procedure)
          new-args
          (procedure-environment procedure)))]
      [else (error 'mapply "Unknown procedure type: ~a" procedure)])))

((global-rules 'putt!) 'define/lazy eval-definition/lazy)

(define genv (make-env `(+ ,+)))

(define s
  '(let ([a 1])
     (define (f x)
       (define b (+ a x))
       (define a 5)
       (+ a b))
     (f 10)))

(@update ([make-procedure make-procedure/lazy]
          [mapply mapply/lazy]
          [lookup-variable-exp lookup-variable-exp/lazy])
  (?== 20 (eval s genv)))

((global-rules 'del!) 'define/lazy)
)



(run-ex 19)