#lang racket
(require (for-syntax racket/syntax syntax/parse))

(provide ex run-ex @>> @update @update! @catch)

(define-syntax (ex stx)
  (syntax-parse stx
    [(_ ex-id outer-expr ... (~literal ------) inner-expr ...)
     (with-syntax ([name (format-id #'ex-id "ex-~a" (syntax->datum #'ex-id))])
       #'(begin
            outer-expr ...
            (define (name)
              (printf "---------- ex ~a ----------\n" ex-id)
              (let () inner-expr ... (void))
              (printf "---------------------------\n\n"))))]
    [(_ ex-id expr ...)
     (with-syntax ([name (format-id #'ex-id "ex-~a" (syntax->datum #'ex-id))])
       #'(define (name)
           (printf "---------- ex ~a ----------\n" ex-id)
           (let () expr ... (void))
           (printf "---------------------------\n\n")))]
    [_ #'(void)]))

(define-syntax (run-ex stx)
  (define (id-list start end)
    (if (> start end)
        '()
        (cons start (id-list (+ start 1) end))))
  (define (run id)
    (with-syntax ([name (format-id stx "ex-~a" id)])
      #'(name)))
  (define (run-list ids)
    #`(begin
        #,@(for/list ([id ids])
             (run id))))
  (define (exclude lst1 lst2)
    (filter (lambda (e) (not (member e lst2))) lst1))
  (syntax-parse stx
    [(_ ex-id-start (~literal ~) ex-id-end (~literal but) n-ex-id-start (~literal ~) n-ex-id-end)
     (run-list
       (exclude
         (id-list (syntax->datum #'ex-id-start)
                  (syntax->datum #'ex-id-end))
         (id-list (syntax->datum #'n-ex-id-start)
                  (syntax->datum #'n-ex-id-end))))]
    [(_ ex-id-start (~literal ~) ex-id-end (~literal but) n-ex-id ...)
     (run-list
       (exclude
         (id-list (syntax->datum #'ex-id-start)
                  (syntax->datum #'ex-id-end))
         (map syntax->datum (syntax->list #'(n-ex-id ...)))))]
    [(_ ex-id-start (~literal ~) ex-id-end)
     (run-list
       (id-list (syntax->datum #'ex-id-start)
                (syntax->datum #'ex-id-end)))]
    [(_ ex-id ...)
     (run-list
       (map syntax->datum (syntax->list #'(ex-id ...))))]
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
