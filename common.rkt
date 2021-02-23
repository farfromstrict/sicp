#lang racket
(require (for-syntax syntax/parse racket/path))

(provide ex ?== ?=== ?~= ?true ?false
         @>> @update @update!
         average int/list)

(define-syntax (ex stx)
  (syntax-parse stx
    [(_ ex-id outer-expr ... (~literal ------) inner-expr ...)
     #'(begin
         outer-expr ...
         (printf "---------- ex ~a ----------\n" ex-id)
         (let () inner-expr ... (void))
         (printf "----------------------------\n\n"))]
    [(_ ex-id expr ...)
     #'(begin
         (printf "---------- ex ~a ----------\n" ex-id)
         (let () expr ... (void))
         (printf "----------------------------\n\n"))]
    [_ #'(void)]))

(define-for-syntax (check-helper stx pred? aexpr eexpr)
  (with-syntax ([stx-file (path->string (file-name-from-path (syntax-source stx)))]
                [stx-line (syntax-line stx)]
                [stx-col (syntax-column stx)])
    #`(let* ([av #,aexpr]
             [ev #,eexpr]
             [result (#,pred? av ev)]
             [status-str (if result "SUCCESS" "FAILURE")]
             [loc-str (format "~a:~a:~a" stx-file stx-line stx-col)]
             [comp-str (if result "==" "!=")])
        (printf "[~a][~a] ~a ~a ~a\n" status-str loc-str #,aexpr comp-str #,eexpr))))

(define-syntax (?== stx)
  (syntax-parse stx
    [(_ aexpr eexpr)
     (check-helper stx equal? #'aexpr #'eexpr)]
    [_ #'(void)]))

(define-syntax (?=== stx)
  (syntax-parse stx
    [(_ aexpr eexpr)
     (check-helper stx eqv? #'aexpr #'eexpr)]
    [_ #'(void)]))

(define-syntax (?~= stx)
  (syntax-parse stx
    [(_ aexpr eexpr tolerance)
     (check-helper
        stx
        #'(lambda (v1 v2)
            (< (abs (- v1 v2))
               tolerance))
        #'aexpr
        #'eexpr)]
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
    [(_ expr)
     #'(printf "~a\n" expr)]
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


(define (average e . w)
  (let ([sum (+ e (apply + w))]
        [cnt (+ 1 (length w))])
    (/ sum cnt)))

(define (int/list low high [step 1])
  (let loop ([n low] [rst '()])
    (if (> n high)
        (reverse rst)
        (loop (+ n step) (cons n rst)))))