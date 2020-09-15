#lang sicp
(#%require racket)
(#%require sicp-pict)

(#%require "common.rkt")


;;; 1.1
((lambda ()
(hlog "ex-1.1")
(assert 10 10)
(assert 12 (+ 5 3 4))
(assert 8 (- 9 1))
(assert 3 (/ 6 2))
(assert 6 (+ (* 2 4) (- 4 6)))

(define a 3)
(define b (+ a 1))

(assert 19 (+ a b (* a b)))
(assert #f (= a b))
(assert 4
  (if (and (> b a) (< b (* a b)))
      b
      a))
(assert 16
  (cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25)))
(assert 6
  (+ 2 (if (> b a) b a)))
(assert 16
  (* (cond ((> a b) a)
           ((< a b) b)
           (else -1))
     (+ a 1)))
(tlog)
))

;;; 1.2
((lambda ()
(define a
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(hlog "ex-1.2")
(assert (/ 148 -600) a)
(tlog)
))
