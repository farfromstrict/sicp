#lang sicp
(#%require racket)
(#%require sicp-pict)
(#%require "common.rkt")


;;; racket&sicp test
((lambda ()
  (define s (stream-cons 1 (stream-map inc s)))

  (define p1 (cons 1 2))
  (define p2 (mcons 1 2))
  (set-car! p2 10)
  (set-cdr! p2 20)

  (define l1 (list 1 2 3))
  (define ml1 (mlist 1 2 3))
  (set-car! ml1 10)

  (mlog "racket&sicp test"
    pi
    (identity 100)
    (inc 10)
    (dec 20)
    (stream-first s)
    (stream-ref s 10)
    (average 5)
    (average 1 2 4.0)
    p1
    p2
    l1
    ml1
    nil
    empty
    (filter odd? '(1 2 3 4 5 6)))
))


;;; Declaring Procedures
((lambda ()
(define (test-rest-args . w) w)

(define (test-optional-args a [b 2])
  (list a b))

(define (test-keyword-args a #:b b)
  (list a b))

(define (test-optional-keyword-args a #:b [b 2])
  (list a b))

(define (test-rest-keyword-args #:a [a 1] b . w)
  (list (cons 'a a) (cons 'b b) w))

(mlog "Declaring Procedures"
  (test-rest-args)
  (test-rest-args 1)
  (test-rest-args 1 2 3)
  (test-optional-args 1)
  (test-optional-args 1 3)
  (test-keyword-args 1 #:b 2)
  (test-keyword-args #:b 1 2)
  (test-optional-keyword-args 1)
  (test-optional-keyword-args 1 #:b 3)
  (test-rest-keyword-args 2 3 4 5)
  (test-rest-keyword-args #:a 10 2 3 4 5)
  (test-rest-keyword-args 2 3 4 5 #:a 100)
)
))
