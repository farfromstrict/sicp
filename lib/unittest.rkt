#lang racket
(require "chalk.rkt"
         (for-syntax racket/syntax syntax/parse racket/path))

(provide ?== ?!= ?=== ?!== ?~= ?~=% ?true ?false)

(define-for-syntax (check-helper stx pred? aexpr eexpr [pass-op "=="] [fail-op "!="] [dproc #f])
  (with-syntax ([stx-file (path->string (file-name-from-path (syntax-source stx)))]
                [stx-line (syntax-line stx)]
                [stx-col (syntax-column stx)])
    #`(let* ([av #,aexpr]
             [ev #,eexpr]
             [result (#,pred? av ev)]
             [status-str (if result
                           (chalk "SUCCESS" #:fg-color 'green #:effect 'bold)
                           (chalk "FAILURE" #:fg-color 'red #:effect 'bold))]
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
        [else (abs (/ (- v1 v2) (/ sum 2.0)))])))
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
