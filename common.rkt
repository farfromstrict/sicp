#lang sicp
(#%require racket)
(#%require sicp-pict)

(provide (all-defined-out))

;;; built-in procedures
(define (mlist . w)
  (define (rec lst)
    (cond
      [(null? lst) empty]
      [else (mcons (car lst) (rec (cdr lst)))]))
  (rec w))

(define square sqr)


;;; common tools
(define (average a . w)
  (/ (apply + (cons a w)) (+ (length w) 1)))

(define (mprintln . w)
  (map displayln w)
  (printf ""))

(define (hlog idx)
  (printf "[~a]\n----------\n" idx))

(define (tlog)
  (printf "----------\n\n"))

(define (mlog idx . w)
  (hlog idx)
  (map displayln w)
  (tlog))

(define (assert x y)
  (if (equal? x y)
    (printf "[   OK] ~a == ~a\n" x y)
    (printf "[ERROR] ~a != ~a\n" x y)))

(define (assert-close-enough x y [t 1e-3])
  (define (diff-ratio a b)
    (if (= a b)
        0
        (/ (abs (- a b)) (/ (+ a b) 2))))
  (let ([dr (diff-ratio x y)])
    (if (< dr t)
      (printf "[   OK] ~a ~~= ~a (~a)\n" x y dr)
      (printf "[ERROR] ~a != ~a (~a)\n" x y dr))))


; syntax extensions
(define-syntax display-error
  (syntax-rules ()
    [(_ b1 b2 ...)
     (with-handlers ([(lambda (v) #t) (lambda (v) (exn-message v))]) b1 b2 ...)]))