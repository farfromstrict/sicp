#lang racket
(require (for-syntax racket/syntax syntax/parse))

(provide ex run-ex @>> @update @update! @catch @alias)

(define-syntax (ex stx)
  (define (define? exp)
    (let ([lst (syntax-e exp)])
      (cond
        [(pair? lst) (eq? 'define (syntax->datum (car lst)))]
        [else #f])))
  (define (split exp)
    (syntax-case exp (define)
      [(define ((id ps1 ...) ps2 ...) body ...)
       (cons #'id #`(lambda (ps1 ...) (lambda (ps2 ...) body ...)))]
      [(define (id params ...) body ...)
       (cons #'id #`(lambda (params ...) body ...))]
      [(define (id ps1 ... . ps2) body ...)
       (cons #'id #`(lambda (ps1 ... . ps2) body ...))]
      [(define id val) (cons #'id #'val)]))
  (define (scan-defines exps)
    (let loop ([exps exps] [defs '()] [no-defs '()])
      (cond
        [(null? exps)
         (cond
           [(null? defs) (cons defs no-defs)]
           [else (cons (map split defs) no-defs)])]
        [(define? (car exps)) (loop (cdr exps) `(,@defs ,(car exps)) no-defs)]
        [else (loop (cdr exps) defs `(,@no-defs ,(car exps)))])))
  (syntax-parse stx
    [(_ ex-id outter-expr ... (~literal ------) inner-expr ...)
     (with-syntax ([outter-name (format-id #'ex-id "outter-ex-~a" (syntax->datum #'ex-id))]
                   [inner-name  (format-id #'ex-id "inner-ex-~a"  (syntax->datum #'ex-id))])
       (let ([exps-pair (scan-defines (syntax->list #'(outter-expr ...)))])
         (let ([defs (car exps-pair)] [no-defs (cdr exps-pair)])
           (cond
             [(null? defs)
              #`(begin
                  (define (outter-name) #,@no-defs (void))
                  (define (inner-name)
                    (printf "---------- ex ~a ----------\n" ex-id)
                    (let () inner-expr ... (void))
                    (printf "---------------------------\n\n")))]
             [else
              #`(begin
                  #,@(map (lambda (def) #`(define #,(car def) '*unassigned*)) defs)
                  (define (outter-name)
                    #,@(map (lambda (def) #`(set! #,(car def) #,(cdr def))) defs)
                    #,@no-defs
                    (void))
                  (define (inner-name)
                    (printf "---------- ex ~a ----------\n" ex-id)
                    (let () inner-expr ... (void))
                    (printf "---------------------------\n\n")))]))))]
    [(_ ex-id expr ...)
     (with-syntax ([outter-name (format-id #'ex-id "outter-ex-~a" (syntax->datum #'ex-id))]
                   [inner-name  (format-id #'ex-id "inner-ex-~a"  (syntax->datum #'ex-id))])
       #'(begin
           (define (outter-name) (void))
           (define (inner-name)
             (printf "---------- ex ~a ----------\n" ex-id)
             (let () expr ... (void))
             (printf "---------------------------\n\n"))))]
    [_ #'(void)]))

(define-syntax (run-ex stx)
  (define (max lst)
    (let loop ([lst (cdr lst)] [rst (car lst)])
      (cond
        [(null? lst) rst]
        [(> (car lst) rst) (loop (cdr lst) (car lst))]
        [else (loop (cdr lst) rst)])))
  (define (id-list start end)
    (if (> start end) '() (cons start (id-list (+ start 1) end))))
  (define (run id active)
    (with-syntax ([outter-name (format-id stx "outter-ex-~a" id)]
                  [inner-name (format-id stx "inner-ex-~a" id)])
      (let ([outter-ok (identifier-binding #'outter-name)] [inner-ok (identifier-binding #'inner-name)])
        (if active
            #`(begin #,(if outter-ok #'(outter-name) #'(void)) #,(if inner-ok #'(inner-name) #'(void)))
            (if outter-ok #'(outter-name) #'(void))))))
  (define (run-list pairs)
    #`(begin
        #,@(for/list ([pair pairs])
             (run (car pair) (cdr pair)))))
  (define (exclude lst1 lst2)
    (map
      (lambda (id) (cons id (and (member id lst1) (not (member id lst2)))))
      (id-list 1 (max lst1))))
  (syntax-parse stx
    [(_ id-start (~literal ~) id-end (~literal but) n-id-start (~literal ~) n-id-end)
     (let ([lst  (id-list (syntax->datum #'id-start)   (syntax->datum #'id-end))]
           [nlst (id-list (syntax->datum #'n-id-start) (syntax->datum #'n-id-end))])
       (run-list (exclude lst nlst)))]
    [(_ id-start (~literal ~) id-end (~literal but) n-id ...)
     (let ([lst  (id-list (syntax->datum #'id-start) (syntax->datum #'id-end))]
           [nlst (map syntax->datum (syntax->list #'(n-id ...)))])
       (run-list (exclude lst nlst)))]
    [(_ id-start (~literal ~) id-end)
     (let ([lst (id-list (syntax->datum #'id-start) (syntax->datum #'id-end))])
       (run-list (exclude lst '())))]
    [(_ id ...)
     (let ([lst (map syntax->datum (syntax->list #'(id ...)))])
       (run-list (exclude lst '())))]
    [_ #'(void)]))

(define-syntax (@>> stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(begin
         (printf "~a\n" expr)
         ...)]
    [_ #'(void)]))

(define-syntax (@update! stx)
  (syntax-parse stx
    [(_ ([proc-id:id new-expr] ...) body ...)
     #'(begin
         (set! proc-id new-expr) ...
         body ...)]))

(define-syntax (@update stx)
  (syntax-parse stx
    [(_ ([proc-id:id new-expr] ...) body ...)
     #'(begin
         (define stashs (list proc-id ...))
         (set! proc-id new-expr) ...
         body ...
         (define idx 0)
         (begin
           (set! proc-id (list-ref stashs idx))
           (set! idx (+ idx 1))) ...)]))

(define-syntax (@catch stx)
  (syntax-parse stx
    [(_ body ...)
     #'(with-handlers ([(const #t) exn-message]) body ... (void))]
    [_ #'(void)]))

(define-syntax-rule (@alias new-id old-id)
  (define-syntax new-id (make-rename-transformer #'old-id)))