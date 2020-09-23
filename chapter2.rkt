#lang sicp
(#%require racket)
(#%require sicp-pict)

(#%require (only mzscheme fluid-let))

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
(handle-error (div-interval (make-interval 10 20) (make-interval -2 5)))
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


;;; 3.12
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

(mlog "ex-3.12"
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