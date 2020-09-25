#lang sicp
(#%require (all-except racket format))
(#%require sicp-pict)

(#%require (only mzscheme fluid-let))
(#%require (only math/number-theory prime?))

(#%require (only srfi/48 format))

(#%require "common.rkt")


;;; 2.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (cond
    [(= d 0) (error "[make-rat] denom can not be 0" n d)]
    [(= n 0) (cons 0 1)]
    [else
      (let* ([g (gcd n d)] [rn (/ n g)] [rd (/ d g)])
        (if (> rd 0)
          (cons rn rd)
          (cons (- rn) (- rd))))]))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (let ([n (numer x)] [d (denom x)])
    (cond
      [(= d 0) (printf "NaN")]
      [(= n 0) (printf "0")]
      [else (printf "~a/~a" n d)])))

((lambda ()
(hlog "ex-2.1")
(print-rat (make-rat 0 28))
(newline)
(print-rat (make-rat 20 28))
(newline)
(print-rat (make-rat -20 -28))
(newline)
(print-rat (make-rat -20 28))
(newline)
(print-rat (make-rat 20 -28))
(newline)
(tlog)
))


;;; 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (printf "(~a,~a)" (x-point p) (y-point p)))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (midpoint-segment l)
  (let ([ps (start-segment l)] [pe (end-segment l)])
    (let ([psx (x-point ps)]
          [psy (y-point ps)]
          [pex (x-point pe)]
          [pey (y-point pe)])
      (make-point
        (average psx pex)
        (average psy pey)))))

(hlog "ex-2.2")
(define p1 (make-point 10 20))
(define p2 (make-point 30 50))
(define l1 (make-segment p1 p2))
(print-point p1)
(newline)
(print-point p2)
(newline)
(print-point (midpoint-segment l1))
(newline)
(tlog)


;;; 2.3
(hlog "ex-2.3")
(define left-side #f)
(define right-side #f)
(define bottom-side #f)
(define top-side #f)
(define width-rect #f)
(define height-rect #f)

(define (print-rect r)
  (printf "(~a, ~a, ~a, ~a) => (w: ~a, h: ~a)\n"
    (left-side r)
    (right-side r)
    (bottom-side r)
    (top-side r)
    (width-rect r)
    (height-rect r)))

(define (circum-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

((lambda ()
(define (make-rect l r b t)
  (list l r b t))

(set! left-side (lambda (r) (car r)))
(set! right-side (lambda (r) (cadr r)))
(set! bottom-side (lambda (r) (caddr r)))
(set! top-side (lambda (r) (cadddr r)))

(set! width-rect (lambda (r) (- (right-side r) (left-side r))))
(set! height-rect (lambda (r) (- (top-side r) (bottom-side r))))

(define r (make-rect 10 50 20 80))
(printf "~a\n" r)
(print-rect r)
(mprintln (circum-rect r) (area-rect r))
))
(displayln "~~~~~~")

((lambda ()
(define (make-rect ox oy w h)
  (list (cons ox oy) w h))

(set! width-rect (lambda (r) (cadr r)))
(set! height-rect (lambda (r) (caddr r)))

(set! left-side (lambda (r) (caar r)))
(set! right-side (lambda (r) (+ (left-side r) (width-rect r))))
(set! bottom-side (lambda (r) (cdar r)))
(set! top-side (lambda (r) (+ (bottom-side r) (height-rect r))))

(define r (make-rect 10 20 40 60))
(printf "~a\n" r)
(print-rect r)
(mprintln (circum-rect r) (area-rect r))
))
(tlog)


;;; 2.4
((lambda ()
(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))
; => ((lambda (m) (m x y)) (lambda (p q) p))
; => ((lambda (p q) p) x y)
; => x

(define (cdr z)
  (z (lambda (p q) q)))

(define p (cons 'a 'b))
(mlog "ex-2.4"
(car p)
(cdr p)
)
))


;;; 2.5
((lambda ()
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (factor-count n m)
  (let loop ([n n] [rst 0])
    (let ([q (/ n m)])
      (if (integer? q)
          (loop q (inc rst))
          rst))))

(define (car z) (factor-count z 2))
(define (cdr z) (factor-count z 3))

(define p (cons 3 4))
(mlog "ex-2.5"
p
(car p)
(cdr p)
)
))


;;;2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; one = (add-1 zero)
; => (lambda (x) (f ((zero f) x)))
; => (lambda (x) (f ((lambda (x) x) x)))
; => (lambda (x) (f x))

(define one (lambda (f) (lambda (x) (f x))))

; two = (add-1 one)
; => (lambda (x) (f ((one f) x)))
; => (lambda (x) (f ((lambda (x) (f x)) x)))
; => (lambda (x) (f (f x)))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(mlog "ex-2.6"
((zero inc) 10)
((one inc) 10)
((two inc) 10)
(((add-1 two) inc) 10)
(((add one two) inc) 10)
)


;;; 2.7
(define (make-interval a b)
  (if (<= a b)
      (cons a b)
      (cons b a)))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define i (make-interval 10 20))
(mlog "ex-2.7"
i
(upper-bound i)
(lower-bound i)
)


;;; 2.8
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define i1 (make-interval 10 20))
(define i2 (make-interval 3 7))

(mlog "ex-2.8"
(add-interval i1 i2)
(sub-interval i1 i2)
)


;;; 2.9
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(mlog "ex-2.9"
(width i1)
(width i2)
(width (add-interval i1 i2))
(width (sub-interval i1 i2))
(width (mul-interval i1 i2))
(width (div-interval i1 i2))
)


;;; 2.10
(define (cross-zero? i)
  (and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))

(define (div-interval-new x y)
  (if (cross-zero? y)
      (error 'div-interval "divided by interval cross-zero")
      (mul-interval x 
        (make-interval 
          (/ 1.0 (upper-bound y)) 
          (/ 1.0 (lower-bound y))))))

(set! div-interval div-interval-new)

(mlog "ex-2.10"
(cross-zero? (make-interval 0 0))
(cross-zero? (make-interval 0 2))
(cross-zero? (make-interval -1 0))
(cross-zero? (make-interval -1 2))
(cross-zero? (make-interval 1 2))

(div-interval (make-interval 10 20) (make-interval 2 5))
(display-error (div-interval (make-interval 10 20) (make-interval -2 5)))
)


;;; 2.11
(define (mul-interval-new x y)
  (let ([lx (lower-bound x)] [ux (upper-bound x)]
        [ly (lower-bound y)] [uy (upper-bound y)])
    (cond
      [(>= lx 0)
        (cond
          [(>= ly 0) (make-interval (* lx ly) (* ux uy))]
          [(<= uy 0) (make-interval (* ux ly) (* lx uy))]
          [else (make-interval (* ux ly) (* ux uy))])]
      [(<= ux 0)
        (cond
          [(>= ly 0) (make-interval (* lx uy) (* ux ly))]
          [(<= uy 0) (make-interval (* ux uy) (* lx ly))]
          [else (make-interval (* lx uy) (* lx ly))])]
      [else
        (cond
          [(>= ly 0) (make-interval (* lx uy) (* ux uy))]
          [(<= uy 0) (make-interval (* ux ly) (* lx ly))]
          [else (make-interval
                  (min (* lx uy) (* ly ux))
                  (max (* lx ly) (* ux uy)))])])))

(define x1 (make-interval 10 16))
(define x2 (make-interval -16 -10))
(define x3 (make-interval -10 16))

(mlog "ex-2.11"
(mul-interval x1 x1)
(mul-interval-new x1 x1)
(mul-interval x1 x2)
(mul-interval-new x1 x2)
(mul-interval x1 x3)
(mul-interval-new x1 x3)
(mul-interval x2 x2)
(mul-interval-new x2 x2)
(mul-interval x2 x3)
(mul-interval-new x2 x3)
(mul-interval x3 x3)
(mul-interval-new x3 x3)
)


;;; 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ([c (center i)])
    (if (= c 0)
        (error 'interval-percent "interval's center is zero")
        (/ (width i) c))))

((lambda ()
(define i1 (make-center-width 5 0.3))
(define i2 (make-center-percent 5 0.06))

(mlog "ex-2.12"
i1
(center i1)
(width i1)
(percent i1)
"~~~~~~"
i2
(center i2)
(width i2)
(percent i2)
)
))


;;; 2.13
((lambda ()
(define i1 (make-center-percent 16 1e-3))
(define i2 (make-center-percent 27 3e-3))
(define i3 (mul-interval i1 i2))

(mlog "ex-2.13"
; (percent i3) = (percent i1) + (percent i2)
(percent i3)
)
))


;;; 2.14
((lambda ()
(define i1 (make-center-percent 16 1e-3))
(define i2 (make-center-percent 27 3e-3))
(define i3 (div-interval i1 i1))
(define i4 (div-interval i1 i2))

(mlog "ex-2.14"
i3
(percent i3)
i4
(percent i4)
)
))


;;; 2.15
((lambda ()
(define i0 (make-interval 1 1))
(define i1 (make-center-percent 16 1e-3))
(define i2 (make-center-percent 27 3e-3))

(define i3 (div-interval (mul-interval i1 i2) (add-interval i1 i2)))

(define i4 (div-interval i0 (add-interval (div-interval i0 i1) (div-interval i0 i2))))

(mlog "ex-2.15"
; Eva is RIGHT
i3
(percent i3)
i4
(percent i4)
)
))


;;; 2.16
((lambda ()
(define (interval-calc #:steps [steps 1000] #:times [times 1000] f . intvs)
  (define lower-list (map lower-bound intvs))

  (define upper-list (map upper-bound intvs))

  (define delta-list
    (map (lambda (a b)
           (exact->inexact (/ (- a b) steps)))
         upper-list
         lower-list))

  (define (random-in-interval lower delta)
    (if (= delta 0)
      lower
      (+ lower (* delta (random 0 (+ steps 1))))))

  (define (iter remain-times lower upper)
    (cond
      [(= remain-times 0) (make-interval lower upper)]
      [else
        (let* ([vals (map random-in-interval lower-list delta-list)]
               [data (apply f vals)])
          (cond
            [(< data lower) (iter (dec remain-times) data upper)]
            [(> data upper) (iter (dec remain-times) lower data)]
            [else (iter (dec remain-times) lower upper)]))]))

  (let ([guess1 (apply f lower-list)]
        [guess2 (apply f upper-list)])
    (cond
      [(< guess1 guess2) (iter times guess1 guess2)]
      [else (iter times guess2 guess1)]))
)

(define r1 (make-center-percent 6.8 0.1))
(define r2 (make-center-percent 4.7 0.05))
(define r3 (make-center-percent 100 0.001))
(define r4 (make-center-percent 10 0.001))

(define i1 (make-interval 2 4))
(define i2 (make-interval -2 0))
(define i3 (make-interval 3 8))

(mlog "ex-2.16"
(add-interval r1 r2)
(interval-calc + r1 r2)
(interval-calc (lambda (a) (/ a a)) r3)
(interval-calc
  (lambda (a b)
    (/ (+ (/ 1 a) (/ 1 b))))
  r1
  r2)
(interval-calc
  (lambda (a b)
    (/ (* a b) (+ a b)))
  r1
  r2)
(interval-calc
  (lambda (a b)
    (+ (/ a b) (/ b a)))
  (make-interval 1 3)
  (make-interval 2 4))
(interval-calc sqr (make-interval -2 2))
(interval-calc (lambda (a b c) (* a (+ b c))) i1 i2 i3)
(interval-calc (lambda (a b c) (+ (* a b) (* a c))) i1 i2 i3)
(div-interval (make-interval 1 3) (make-interval 2 4))
(interval-calc / (make-interval 1 3) (make-interval 2 4))
)
))


;;; 2.17
((lambda ()
(define (last-pair lst)
  (let iter ([lst lst])
    (if (null? (cdr lst))
        (list (car lst))
        (iter (cdr lst)))))

(mlog "ex-2.17"
(last-pair (list 23 72 149 34))
)
))


;;; 2.18
((lambda ()
(define (reverse lst)
  (let iter ([lst lst] [rst '()])
    (if (null? lst)
        rst
        (iter (cdr lst) (cons (car lst) rst)))))

(mlog "ex-2.18"
(reverse (list 1 4 9 16 25))
)
))


;;; 2.19
((lambda ()
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(mlog "ex-2.19"
(cc 100 us-coins)  ; => 292
(cc 100 uk-coins)
)
))


;;; 2.20
((lambda ()
(define (same-parity first . rest)
  (reverse (let iter ([lst rest] [rst (list first)])
    (if (null? lst)
        rst
        (if (= 0 (remainder (- (car lst) first) 2))
            (iter (cdr lst) (cons (car lst) rst))
            (iter (cdr lst) rst))))))

(mlog "ex-2.20"
(same-parity 1 2 3 4 5 6 7)
; => (1 3 5 7)
(same-parity 2 3 4 5 6 7)
; =>(2 4 6)
)
))


;;; 2.21
((lambda ()
(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (sqr (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map sqr items))

(mlog "ex-2.21"
(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))
; => (1 4 9 16)
)
))


;;; 2.22
((lambda ()
(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))

(mlog "ex-2.22"
(square-list-3 (list 1 2 3 4))
; => (16 9 4 1)
(square-list-4 (list 1 2 3 4))
; => ((((() . 1) . 4) . 9) . 16)
)
))


;;; 2.23
((lambda ()
(define (for-each f lst)
  (if (null? lst)
      #t
      (begin
        (f (car lst))
        (for-each f (cdr lst)))))

(hlog "ex-2.23")
(for-each 
  (lambda (x) (newline) (display x))
  (list 57 321 88))
; => 57
; => 321
; => 88
(newline)
(tlog)
))


;;; 2.24
(mlog "ex-2.24"
(list 1 (list 2 (list 3 4)))
; => (1 (2 (3 4)))
; (_ | _) -> (_ | _) -> (nil)
;  |          |
;  1         (_ | _) -> (_ | _) -> (nil)
;             |          |
;             2         (_ | _) -> (_ | _) -> (nil)
;                        |          |
;                        3          4
;
;  ------------
;  |          |
;  1    ------------
;       |          |
;       2     -----------
;             |         |
;             3         4
)


;;; 2.25
((lambda ()
(define l1 '(1 3 (5 7) 9))
(define l2 '((7)))
(define l3 '(1 (2 (3 (4 (5 (6 7)))))))

(mlog "ex-2.25"
(cadr (caddr l1))
(car (car l2))
(cadr (cadr (cadr (cadr (cadr (cadr l3))))))
)
))


;;; 2.26
((lambda ()
(define x (list 1 2 3))
(define y (list 4 5 6))

(mlog "ex-2.26"
(append x y)
; => (1 2 3 4 5 6)
(cons x y)
; => ((1 2 3) 4 5 6)
(list x y)
; => ((1 2 3) (4 5 6))
)
))


;;; 2.27
((lambda ()
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse lst)
  (let iter ([lst lst] [rst '()])
    (if (null? lst)
        rst
        (let ([first (car lst)] [rest (cdr lst)])
          (if (pair? first)
              (iter rest (cons (deep-reverse first) rst))
              (iter rest (cons first rst)))))))

(mlog "ex-2.27"
x
(reverse x)
(deep-reverse x)
)
))


;;; 2.28
((lambda ()
(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (define (inner tree)
    (let iter ([tree tree] [rst '()])
      (if (null? tree)
          rst
          (let ([first (car tree)] [rest (cdr tree)])
            (if (pair? first)
                (iter rest (append (inner first) rst))
                (iter rest (append (list first) rst)))))))

  (reverse (inner tree))
)

(define (fringe-rec tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (list tree)]
    [else (append (fringe-rec (car tree)) (fringe-rec (cdr tree)))]))

(mlog "ex-2.28"
(fringe x)
(fringe-rec x)
; => (1 2 3 4)
(fringe (list x x))
(fringe-rec (list x x))
; => (1 2 3 4 1 2 3 4)
(fringe '(1 (2) (3 4) (5 6 7) 8))
(fringe-rec '(1 (2) (3 4) (5 6 7) 8))
)
))


;;; 2.29
((lambda ()
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (cadr m))

(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define (is-leaf? s) (number? s))

(define (total-weight m)
  (if (is-leaf? m)
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

(define (balance? m)
  (if (is-leaf? m)
      #t
      (let* ([lb (left-branch m)] [rb (right-branch m)]
             [lm (branch-structure lb)] [rm (branch-structure rb)])
        (and (= (* (total-weight lm) (branch-length lb))
                (* (total-weight rm) (branch-length rb)))
             (balance? lm)
             (balance? rm)))))

; (define (make-mobile left right)
;  (cons left right))
; => (define (right-branch m) (cdr m))

; (define (make-branch length structure)
;  (cons length structure))
; => (define (branch-structure b) (cdr b))

(define m1 (make-mobile (make-branch 10 40) (make-branch 40 10)))
(define m2 (make-mobile (make-branch 10 30) (make-branch 20 20)))
(define m3 (make-mobile (make-branch 10 m1) (make-branch 10 m2)))

(mlog "ex-2.29"
(total-weight m3)  ; => 100
(balance? m1)  ; => #t
(balance? m2)  ; => #f
(balance? m3)  ; => #f
)
))


;;; 2.30
((lambda ()
(define (square-tree t)
  (cond
    [(null? t) '()]
    [(pair? t) (cons (square-tree (car t)) (square-tree (cdr t)))]
    [else (square t)]))

(mlog "ex-2.30"
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; => (1 (4 (9 16) 25) (36 49))
)
))


;;; 2.31
((lambda ()
(define (tree-map f tree)
  (cond
    [(null? tree) '()]
    [(pair? tree) (cons (tree-map f (car tree)) (tree-map f (cdr tree)))]
    [else (f tree)]))

(define (square-tree tree) 
  (tree-map square tree))

(mlog "ex-2.31"
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; => (1 (4 (9 16) 25) (36 49))
)
))


;;; 2.32
((lambda ()
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ls) (cons (car s) ls)) rest)))))

(mlog "ex-2.32"
(subsets '(1 2 3))
; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
)
))


;;; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

((lambda ()
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(mlog "ex-2.33"
(map sqr '(1 2 3 4 5))
(append '(1 2 3) '(4 5 6))
(length '(1 (2 3) 4))
)
))


;;; 2.34
((lambda ()
(define (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(mlog "ex-2.34"
; 1 + 3x + 5x^3 + x^5
(horner-eval 2 (list 1 3 0 5 0 1))  ; => 79
)
))


;;; 2.35
((lambda ()
(define (count-leaves t)
  (accumulate
    +
    0
    (map
      (lambda (e)
        (cond
          [(null? e) 0]
          [(pair? e) (count-leaves e)]
          [else 1]))
      t)))

(mlog "ex-2.35"
(count-leaves '())  ; => 0
(count-leaves '(1 2 3))  ; => 3
(count-leaves '(1 (2 3) (4) 5 (6 7 8)))  ; => 8
)
))


;;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

((lambda ()
(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(mlog "ex-2.36"
(accumulate-n + 0 s)  ; => (22 26 30)
)
))


;;; 2.37
((lambda ()
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define v1 '(1 2 3 4))
(define v2 '(4 5 6 6))
(define v3 '(6 7 8 9))
(define m (list v1 v2 v3))
(define n '((1 4 6) (2 5 7) (3 6 8) (4 6 9)))

(mlog "ex-2.37"
(dot-product v1 v2)  ; => 56
(matrix-*-vector m v1)  ; => (30 56 80)
(transpose m)  ; => n
(transpose n)  ; => m
(matrix-*-matrix m n)
(matrix-*-matrix n m)
)
))


;;; 2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(mlog "ex-2.38"
(fold-right / 1 (list 1 2 3))  ; => 3/2
(fold-left  / 1 (list 1 2 3))  ; => 1/6
(fold-right list nil (list 1 2 3))  ; => (1 (2 (3 ())))
(fold-left  list nil (list 1 2 3))  ; => (((() 1) 2) 3)
)


;;; 2.39
((lambda ()
(define (reverse-1 sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-2 sequence)
  (fold-left 
   (lambda (x y) (cons y x)) nil sequence))

(mlog "ex-2.39"
(reverse-1 '(1 2 3 4 5 6))
(reverse-2 '(1 2 3 4 5 6))
)
))


;;; 2.40
(define (enumerate-interval low high)
  (reverse (let iter ([num low] [rst '()])
    (if (> num high)
      rst
      (iter (+ num 1) (cons num rst))))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map
        (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map
    make-pair-sum
    (filter prime-sum? (unique-pairs n))))

(mlog "ex-2.40"
(prime-sum-pairs 10)
)


;;; 2.41
(define (unique-triples n)
  (flatmap
    (lambda (i)
      (map
        (lambda (p) (cons i p))
        (unique-pairs (- i 1))))
    (enumerate-interval 1 n)))

(define (spec-sum-triples n s)
  (filter
    (lambda (t) (= s (+ (car t) (cadr t) (caddr t))))
    (unique-triples n)))

(mlog "ex-2.41"
(spec-sum-triples 10 20)
)


;;; 2.42
(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons k new-row) rest-of-queens))

(define (safe? k positions)
  (define (unsafe? pos1 pos2)
    (let ([c1 (car pos1)] [r1 (cdr pos1)] [c2 (car pos2)] [r2 (cdr pos2)])
      (or (= r1 r2)
          (= (abs (- r1 r2)) (abs (- c1 c2))))))
  (define (check new-pos done-poses)
    (cond
      [(null? done-poses) #t]
      [(unsafe? new-pos (car done-poses)) #f]
      [else (check new-pos (cdr done-poses))]))
  (check (car positions) (cdr positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens-1 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

((lambda ()
(hlog "ex-2.42")
(define board-size 8)
(define stm1 (runtime))
; (define slns1 (queens board-size))
(define stm2 (runtime))
; (for-each displayln slns1)
(define cost1 (- stm2 stm1))

(define stm3 (runtime))
; (define slns2 (queens-1 board-size))
(define stm4 (runtime))
(define cost2 (- stm4 stm3))

(printf "~a, ~a, ~a\n" (format "~1,2F" (/ cost2 cost1)) cost1 cost2)
(tlog)
))


;;; 2.43
; Theory: 8^8T
; Real: ~2700T


;;; 2.44 ~ 2.52
; ignored


;;; 2.53
(mlog "ex-2.53"
(list 'a 'b 'c)  ; => (a b c)
(list (list 'george))  ; => ((george))
(cdr '((x1 x2) (y1 y2)))  ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2)))  ; => (y1 y2)
(pair? (car '(a short list)))  ; => #f
(memq 'red '((red shoes) (blue socks)))  ; => #f
(memq 'red '(red shoes blue socks))  ; => (red shoes blue socks)
)


;;; 2.54
((lambda ()
(define (equal? lst1 lst2)
  (cond
    [(and (null? lst1) (null? lst2)) #t]
    [(or (null? lst1) (null? lst2)) #f]
    [(eqv? (car lst1) (car lst2)) (equal? (cdr lst1) (cdr lst2))]
    [else #f]))

(mlog "ex-2.54"
; => #t
(equal? '(this is a list) 
        '(this is a list))

; => #f
(equal? '(this is a list) 
        '(this (is a) list))
)
))


;;; 2.55
(mlog "ex-2.55"
;(car '(quote abracadabra))
(car ''abracadabra)  ; => quote
)


;;; 2.56
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eqv? v1 v2)))

(define (sum? x)
  (and (pair? x) (eqv? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eqv? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eqv? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (=number? e n)
  (and (number? e) (= e n)))

(define (make-sum a1 a2)
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list '+ a1 a2)]))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list '* m1 m2)]))

(define (make-exponentiation b e)
  (cond
    [(=number? e 0) 1]
    [(=number? e 1) b]
    [(=number? b 0) 0]
    [(=number? b 1) 1]
    [(and (number? b) (number? e)) (expt b e)]
    [else (list '** b e)]))

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp)
      (if (same-variable? exp var) 1 0)]
    [(sum? exp)
      (make-sum
        (deriv (addend exp) var)
        (deriv (augend exp) var))]
    [(product? exp)
      (let ([u (multiplier exp)] [v (multiplicand exp)])
        (make-sum
          (make-product u (deriv v var))
          (make-product (deriv u var) v)))]
    [(exponentiation? exp)
      (let ([b (base exp)] [e (exponent exp)])
        (make-product
          (make-product
            e
            (make-exponentiation b (make-sum e -1)))
          (deriv b var)))]
    [else (error 'deriv "Unknown expression type" exp)]))

((lambda ()
(mlog "ex-2.56"
(deriv '(+ x 3) 'x)  ; => 1
(deriv '(* x y) 'x)  ; => y
(deriv '(* (* x y) (+ x 3)) 'x)  ; => 2xy + 3y
(deriv '(** (+ (* x y) 3) y) 'x)  ; => y * y * (xy+3)^(y-1)
(deriv '(** x 3) 'x)  ; => 3x^2
)
))


;;; 2.57
((lambda ()
(define (make-sum-new . args)
  (let* ([num-args (filter number? args)]
         [var-args (filter (lambda (a) (not (number? a))) args)]
         [num-sum (apply + num-args)]
         [var-count (length var-args)])
    (cond
      [(= var-count 0) num-sum]
      [(= num-sum 0)
        (if (> var-count 1)
            (cons '+ var-args)
            (car var-args))]
      [else (cons '+ (cons num-sum var-args))])))

(define (addend-new s) (cadr s))
(define (augend-new s) (apply make-sum-new (cddr s)))

(define (make-product-new . args)
  (let* ([num-args (filter number? args)]
         [var-args (filter (lambda (a) (not (number? a))) args)]
         [num-product (apply * num-args)]
         [var-count (length var-args)])
    (cond
      [(= num-product 0) 0]
      [(= var-count 0) num-product]
      [(= num-product 1)
        (if (> var-count 1)
            (cons '* var-args)
            (car var-args))]
      [else (cons '* (cons num-product var-args))])))

(define (multiplier-new p) (cadr p))
(define (multiplicand-new p) (apply make-product-new (cddr p)))

(set! make-sum make-sum-new)
(set! addend addend-new)
(set! augend augend-new)

(set! make-product make-product-new)
(set! multiplier multiplier-new)
(set! multiplicand multiplicand-new)

(mlog "ex-2.57"
(deriv '(* x y (+ x 3)) 'x)  ; => 2xy + 3y
(deriv '(* x y (+ x y 3)) 'x)  ; => 2xy + 3y + y^2
)
))


;;; 2.58
((lambda ()
(define (sum-new? exp)
  (and
    (pair? exp)
    (pair? (cdr exp))
    (eqv? (cadr exp) '+)))

(define (make-sum-new a1 a2)
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list a1 '+ a2)]))

(define (addend-new exp) (car exp))
(define (augend-new exp) (caddr exp))

(set! sum? sum-new?)
(set! make-sum make-sum-new)
(set! addend addend-new)
(set! augend augend-new)

(define (product-new? exp)
  (and
    (pair? exp)
    (pair? (cdr exp))
    (eqv? (cadr exp) '*)))

(define (make-product-new m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list m1 '* m2)]))

(define (multiplier-new exp) (car exp))
(define (multiplicand-new exp) (caddr exp))

(set! product? product-new?)
(set! make-product make-product-new)
(set! multiplier multiplier-new)
(set! multiplicand multiplicand-new)

(mlog "ex-2.58(a)"
(deriv '(x + (3 * (x + (y + 2)))) 'x)  ; => 4
)
))

((lambda ()
(define (bmemv e lst)
  (reverse (let iter ([lst lst] [rst '()])
    (cond
      [(null? lst) rst]
      [(eqv? (car lst) e) rst]
      [else (iter (cdr lst) (cons (car lst) rst))]))))

(define (unlist lst)
  (if (= 1 (length lst)) (car lst) lst))

(define (insert-between e lst)
  (reverse (let iter ([lst lst] [rst '()])
    (cond
      [(null? (cdr lst)) (cons (car lst) rst)]
      [else (iter (cdr lst) (append (list e (car lst)) rst))]))))

(define (sum-new? exp)
  (and
    (pair? exp)
    (if (memv '+ exp) #t #f)))

(define (make-sum-new . args)
  (let* ([num-args (filter number? args)]
         [var-args (filter (lambda (a) (not (number? a))) args)]
         [num-sum (apply + num-args)]
         [var-count (length var-args)])
    (cond
      [(= var-count 0) num-sum]
      [(= num-sum 0)
        (if (> var-count 1)
            (insert-between '+ var-args)
            (car var-args))]
      [else (insert-between '+ (cons num-sum var-args))])))

(define (addend-new exp) (unlist (bmemv '+ exp)))
(define (augend-new exp) (unlist (cdr (memv '+ exp))))

(set! sum? sum-new?)
(set! make-sum make-sum-new)
(set! addend addend-new)
(set! augend augend-new)

(define (product-new? exp)
  (and
    (pair? exp)
    (memv '* exp)
    (not (memv '+ exp))))

(define (make-product-new . args)
  (let* ([num-args (filter number? args)]
         [var-args (filter (lambda (a) (not (number? a))) args)]
         [num-product (apply * num-args)]
         [var-count (length var-args)])
    (cond
      [(= num-product 0) 0]
      [(= var-count 0) num-product]
      [(= num-product 1)
        (if (> var-count 1)
            (insert-between '* var-args)
            (car var-args))]
      [else (insert-between '* (cons num-product var-args))])))

(define (multiplier-new exp) (unlist (bmemv '* exp)))
(define (multiplicand-new exp) (unlist (cdr (memv '* exp))))

(set! product? product-new?)
(set! make-product make-product-new)
(set! multiplier multiplier-new)
(set! multiplicand multiplicand-new)

(mlog "ex-2.58(b)"
(deriv '(x + x + y + 3) 'x)  ; => 2
(deriv '(x + 3 * (x + y + 2)) 'x)  ; => 4
(deriv '((x + 3) * (x + y + 2)) 'x)  ; => 2x + y + 5
(deriv '(x * y * (x + 3)) 'x)  ; => 2xy + 3y
(deriv '(x * y * x + 3) 'x)  ; => 2xy
)
))


;;; 2.59
((lambda ()
(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

(define (intersection-set s1 s2)
  (cond
    [(or (null? s1) (null? s2)) '()]
    [(element-of-set? (car s1) s2)
       (cons (car s1) (intersection-set (cdr s1) s2))]
    [else (intersection-set (cdr s1) s2)]))

(define (union-set s1 s2)
  (cond
    [(null? s1) s2]
    [(element-of-set? (car s1) s2) (union-set (cdr s1) s2)]
    [else (cons (car s1) (union-set (cdr s1) s2))]))

(define s1 '(1 2 3 4 5))
(define s2 '(3 4 5 6 7))

(mlog "ex-2.59"
(intersection-set s1 s2)
(union-set s1 s2)
)
))


;;; 2.60
((lambda ()
(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? (car set) x) #t]
    [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set s1 s2)
  (cond
    [(or (null? s1) (null? s2)) '()]
    [(element-of-set? (car s1) s2)
       (cons (car s1) (intersection-set (cdr s1) s2))]
    [else (intersection-set (cdr s1) s2)]))

(define (union-set s1 s2) (append s1 s2))

(define s1 '(2 3 2 1 3 2 2))
(define s2 '(2 3 4 5))

(mlog "ex-2.60"
(element-of-set? 1 s1)  ; => #t
(element-of-set? 3 s1)  ; => #t
(element-of-set? 4 s1)  ; => #f
(adjoin-set 1 s1)  ; => (1 2 3 2 1 3 2 2)
(adjoin-set 4 s1)  ; => (4 2 3 2 1 3 2 2)
(intersection-set s1 s2)
(union-set s1 s2)
)
))


;;; 2.61
((lambda ()
(define (adjoin-set x set)
  (if (null? set)
    (cons x '())
    (let ([first (car set)])
      (cond
        [(= x first) set]
        [(< x first) (cons x set)]
        [else (cons first (adjoin-set x (cdr set)))]))))

(mlog "ex-2.61"
(adjoin-set 10 '())
(adjoin-set 10 '(1 3 5 7))
(adjoin-set 3 '(1 3 5 7))
(adjoin-set 4 '(1 3 5 7))
)
))


;;; 2.62
((lambda ()
(define (union-set s1 s2)
  (cond
    [(null? s1) s2]
    [(null? s2) s1]
    [else
      (let ([h1 (car s1)] [h2 (car s2)])
        (cond
          [(= h1 h2) (cons h1 (union-set (cdr s1) (cdr s2)))]
          [(< h1 h2) (cons h1 (union-set (cdr s1) s2))]
          [else (cons h2 (union-set s1 (cdr s2)))]))]))

(define s1 '(1 3 5 7))
(define s2 '(2 4 6 8))
(define s3 '(1 5 9 11))

(mlog "ex-2.62"
(union-set s1 s2)
(union-set s1 s3)
)
))


;;; 2.63
(define (make-tree e lb rb) (list e lb rb))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

((lambda ()
(mlog "ex-2.63"
(tree->list-1 t1)
(tree->list-2 t1)
(tree->list-1 t2)
(tree->list-2 t2)
(tree->list-1 t3)
(tree->list-2 t3)
)
))


;;; 2.64
(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons nil elts)
    (let* ([left-size (quotient (- n 1) 2)]
           [left-result (partial-tree elts left-size)]
           [left-tree (car left-result)]
           [non-left-elts (cdr left-result)]
           [this-entry (car non-left-elts)]
           [right-size (- n (+ left-size 1))]
           [right-result (partial-tree (cdr non-left-elts) right-size)]
           [right-tree (car right-result)]
           [remaining-elts (cdr right-result)])
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))

; partial-tree => (cons tree remaining-elts)
; partial-tree left-size => (cons left-tree other-elts)
; other-elts => entry + right-elts
; partial-tree righ-size => (cons right-tree '())
; => (entry left-tree right-tree)
;
; O(logN)

((lambda ()
(mlog "ex-2.64"
; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
(list->tree '(1 3 5 7 9 11))
)
))


;;; 2.65
((lambda ()
(define tree->list tree->list-2)

(define (union-set-list lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [else
      (let ([h1 (car lst1)] [h2 (car lst2)])
        (cond
          [(= h1 h2) (cons h1 (union-set-list (cdr lst1) (cdr lst2)))]
          [(< h1 h2) (cons h1 (union-set-list (cdr lst1) lst2))]
          [else (cons h2 (union-set-list lst1 (cdr lst2)))]))]))

(define (intersection-set-list lst1 lst2)
  (cond
    [(or (null? lst1) (null? lst2)) '()]
    [else
      (let ([h1 (car lst1)] [h2 (car lst2)])
        (cond
          [(= h1 h2) (cons h1 (intersection-set-list (cdr lst1) (cdr lst2)))]
          [(< h1 h2) (intersection-set-list (cdr lst1) lst2)]
          [else (intersection-set-list lst1 (cdr lst2))]))]))

(define (op-set-tree op-set-list tr1 tr2)
  (let* ([lst1 (tree->list tr1)]
         [lst2 (tree->list tr2)]
         [rst (op-set-list lst1 lst2)])
    (list->tree rst)))

(define (union-set-tree tr1 tr2)
  (op-set-tree union-set-list tr1 tr2))

(define (intersection-set-tree tr1 tr2)
  (op-set-tree intersection-set-list tr1 tr2))

(define l1 '(1 2 3 4))
(define l2 '(5 6 7 8))
(define l3 '(2 3 4 5))
(define l4 '(1 3 5 7))
(define l5 '(2 4 6 8))

(define t1 (list->tree l1))
(define t2 (list->tree l2))
(define t3 (list->tree l3))
(define t4 (list->tree l4))
(define t5 (list->tree l5))

(define (test tr1 tr2)
  (define (inner-print prefix tr)
    (printf "[~a]\n~a\n~a\n" prefix tr (tree->list tr)))

  (define ustr (union-set-tree tr1 tr2))
  (define istr (intersection-set-tree tr1 tr2))

  (inner-print "T1" tr1)
  (inner-print "T2" tr2)
  (inner-print "US" ustr)
  (inner-print "IS" istr)
  (printf "~~~~~~\n"))

(hlog "ex-2.65")
(test t1 t2)
(test t1 t3)
(test t3 t4)
(test t4 t5)
(tlog)
))


;;; 2.66
((lambda ()
(define (key entry) (car entry))

(define (lookup given-key set)
  (cond
    [(null? set) #f]
    [else
      (let* ([entry (entry set)]
             [lb (left-branch set)]
             [rb (right-branch set)]
             [ekey (key entry)])
        (cond
          [(= given-key ekey) entry]
          [(< given-key ekey) (lookup given-key lb)]
          [else (lookup given-key rb)]))]))

(define s1 '((5 50) ((2 20) ((1 10) () ()) ((3 30) () ())) ((7 70) ((6 60) () ()) ((8 80) () ()))))

(mlog "ex-2.66"
(lookup 3 s1)  ; => (3 30)
(lookup 4 s1)  ; => #f
(lookup 7 s1)  ; => (7 70)
(lookup 10 s1)  ; => #f
)
))