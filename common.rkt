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