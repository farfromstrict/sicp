#lang racket
(require (for-syntax racket/syntax syntax/parse racket/path))

(provide ex run-ex
         ?== ?!= ?=== ?!== ?~= ?~=% ?true ?false
         @>> @update @update! @catch
         average int/list diff-ratio)

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

(define-for-syntax (check-helper stx pred? aexpr eexpr [pass-op "=="] [fail-op "!="] [dproc #f])
  (with-syntax ([stx-file (path->string (file-name-from-path (syntax-source stx)))]
                [stx-line (syntax-line stx)]
                [stx-col (syntax-column stx)])
    #`(let* ([av #,aexpr]
             [ev #,eexpr]
             [result (#,pred? av ev)]
             [status-str (if result "SUCCESS" "FAILURE")]
             [comp-str (if result #,pass-op #,fail-op)]
             [loc-str (format "~a:~a:~a" stx-file stx-line stx-col)]
             [dp #,dproc])
        (printf "[~a][~a] ~a ~a ~a" status-str loc-str av comp-str ev)
        (if dp
            (printf " (~a)\n" (dp av ev))
            (printf "\n")))))

(define-syntax (?== stx)
  (syntax-parse stx
    [(_ aexpr eexpr)
     (check-helper stx equal? #'aexpr #'eexpr)]
    [_ #'(void)]))

(define-syntax (?!= stx)
  (syntax-parse stx
    [(_ aexpr eexpr)
     (check-helper stx (lambda (v1 v2) (not (equal? v1 v2))) #'aexpr #'eexpr "!=" "==")]
    [_ #'(void)]))

(define-syntax (?=== stx)
  (syntax-parse stx
    [(_ aexpr eexpr)
     (check-helper stx eqv? #'aexpr #'eexpr)]
    [_ #'(void)]))

(define-syntax (?!== stx)
  (syntax-parse stx
    [(_ aexpr eexpr)
     (check-helper stx (lambda (v1 v2) (not (eqv? v1 v2))) #'aexpr #'eexpr "!=" "==")]
    [_ #'(void)]))

(define-syntax (?~= stx)
  (define (diff v1 v2) (abs (- v1 v2)))
  (define ((close? tolerance) v1 v2) (< (diff v1 v2) tolerance))
  (define (helper tolerance aexpr eexpr)
    (check-helper stx (close? tolerance) aexpr eexpr "~=" "!=" diff))
  (syntax-parse stx
    [(_ aexpr eexpr tolerance:number)
     (helper (syntax->datum #'tolerance) #'aexpr #'eexpr)]
    [(op aexpr eexpr)
     (helper 1e-3 #'aexpr #'eexpr)]
    [_ #'(void)]))

(define-syntax (?~=% stx)
  (define (diff-ratio v1 v2)
    (let ([sum (+ v1 v2)])
      (cond
        [(= sum 0) (abs v1)]
        [else (abs (/ (abs (- v1 v2)) (/ sum 2.0)))])))
  (define ((close? tolerance) v1 v2)
    (< (diff-ratio v1 v2) tolerance))
  (define (helper tolerance aexpr eexpr)
    (check-helper stx (close? tolerance) aexpr eexpr "~=" "!=" diff-ratio))
  (syntax-parse stx
    [(_ aexpr eexpr tolerance:number)
     (helper (syntax->datum #'tolerance) #'aexpr #'eexpr)]
    [(op aexpr eexpr)
     (helper 1e-6 #'aexpr #'eexpr)]
    [_ #'(void)]))

(define-syntax (?true stx)
  (syntax-parse stx
    [(_ expr)
     (check-helper stx equal? #'expr #t)]
    [_ #'(void)]))

(define-syntax (?false stx)
  (syntax-parse stx
    [(_ expr)
     (check-helper stx equal? #'expr #f)]
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


(define (average e . w)
  (let ([sum (+ e (apply + w))]
        [cnt (+ 1 (length w))])
    (/ sum cnt)))

(define (int/list low high [step 1])
  (let loop ([n low] [rst '()])
    (if (> n high)
        (reverse rst)
        (loop (+ n step) (cons n rst)))))

(define (diff-ratio v1 v2)
  (let ([sum (+ v1 v2)])
    (cond
      [(= sum 0) (abs v1)]
      [else (abs (/ (- v1 v2) (/ sum 2)))])))