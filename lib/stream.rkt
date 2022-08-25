#lang racket
(require (for-syntax racket/syntax syntax/parse))

(provide stream-cons/sicp stream-cons/sicp/nm stream-cons/racket delay/s delay/s/nm)

(define-syntax (stream-cons/sicp stx)
  (syntax-parse stx
    [(_ a b)
     #'(cons a (delay/s b))]
    [_ #'(void)]))

(define-syntax (stream-cons/sicp/nm stx)
  (syntax-parse stx
    [(_ a b)
     #'(cons a (delay/s/nm b))]
    [_ #'(void)]))

(define-syntax (stream-cons/racket stx)
  (syntax-parse stx
    [(_ a b)
     #'(cons (delay/s a) (delay/s b))]
    [_ #'(void)]))

(define-syntax (delay/s stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(let ([already-run? #f] [result #f])
         (lambda ()
           (cond
             [already-run? result]
             [else
              (set! result ((lambda () expr ...)))
              (set! already-run? #t)
              result])))]
    [_ #'(void)]))

(define-syntax (delay/s/nm stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(lambda () expr ...)]
    [_ #'(void)]))