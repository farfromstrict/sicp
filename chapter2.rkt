#lang sicp
(#%require "common.rkt")
(#%require racket
           (only math/number-theory prime?))


;;; ex 2.1
(ex 1
(define (make-rat n d)
  (cond
    [(= d 0) (error 'make-rat "denom can not be zero")]
    [(= n 0) (cons 0 1)]
    [else
      (let* ([g (gcd n d)] [rn (/ n g)] [rd (/ d g)])
        (cond
          [(> rd 0) (cons rn rd)]
          [else (cons (- rn) (- rd))]))]))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (printf "~a/~a\n" (numer x) (denom x)))

(define (test n d)
  (define r (make-rat n d))
  (print-rat r))

(test 12 40)
(test -12 40)
(test 12 -40)
(test -12 -40)
)


;;; ex 2.2
(ex 2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (printf "(~a, ~a)" (x-point p) (y-point p)))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (define sp (start-segment s))
  (define ep (end-segment s))
  (make-point (average (x-point sp) (x-point ep))
              (average (y-point sp) (y-point ep))))

(define p1 (make-point 10 20))
(define p2 (make-point 30 50))
(define s1 (make-segment p1 p2))
(define mp (midpoint-segment s1))

(print-point mp) (newline)
(?== 20 (x-point mp))
(?== 35 (y-point mp))
)


;;; ex 2.3
(ex 3
(define make-rect #f)
(define rect-width #f)
(define rect-height #f)

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

(define (rect-circum r)
  (* 2 (+ (rect-width r) (rect-height r))))

(@update ([make-rect
           (lambda (lbx lby width height)
             (cons (cons lbx lby) (cons width height)))]
          [rect-width (lambda (r) (cadr r))]
          [rect-height (lambda (r) (cddr r))])
  (define rect1 (make-rect 0 0 10 20))
  (?== 200 (rect-area rect1))
  (?== 60 (rect-circum rect1)))

(@update ([make-rect
           (lambda (l r t b)
             (list l r t b))]
          [rect-width
           (lambda (r)
             (- (cadr r) (car r)))]
          [rect-height
           (lambda (r)
             (- (caddr r) (cadddr r)))])
  (define rect2 (make-rect 0 10 20 0))
  (?== 200 (rect-area rect2))
  (?== 60 (rect-circum rect2)))
)


;;; ex 2.4
(ex 4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 10 20))
(?== 10 (car z))
(?== 20 (cdr z))
)


;;; ex 2.5
(ex 5
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (factor-count n d)
  (let loop ([n n] [rst 0])
    (if (= 0 (remainder n d))
        (loop (quotient n d) (+ rst 1))
        rst)))

(define (car z) (factor-count z 2))

(define (cdr z) (factor-count z 3))

(define z (cons 3 4))
(?== 3 (car z))
(?== 4 (cdr z))
)


;;; ex 2.6
(ex 6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
; one = (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))

(define two (lambda (f) (lambda (x) (f (f x)))))
; two = (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define (+ n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(?== 100 ((zero inc) 100))
(?== 101 ((one inc) 100))
(?== 102 ((two inc) 100))

(define three (+ one two))
(?== 103 ((three inc) 100))

(define five (+ two three))
(?== 105 ((five inc) 100))
)


;;; ex 2.7
(ex 7
(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (max (car z) (cdr z)))

(define (lower-bound z)
  (min (car z) (cdr z)))

(define (interval->string z)
  (format "[~a, ~a]" (lower-bound z) (upper-bound z)))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let* ([lx (lower-bound x)] [ux (upper-bound x)]
         [ly (lower-bound y)] [uy (upper-bound y)]
         [p1 (* lx ly)] [p2 (* lx uy)] [p3 (* ux ly)] [p4 (* ux uy)])
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y)))))
------
(define z1 (make-interval 10 20))
(?== 10 (lower-bound z1))
(?== 20 (upper-bound z1))

(define z2 (make-interval 20 10))
(?== 10 (lower-bound z2))
(?== 20 (upper-bound z2))
)


;;; ex 2.8
(ex 8
(@update! ([make-interval
            (lambda (a b)
              (if (> a b) (cons b a) (cons a b)))]
           [lower-bound car]
           [upper-bound cdr]))

;; 对于任意 vx (vx 属于区间 x) 和任意 vy (vy 属于区间 y)
;; f(x, y) 定义为 f(vx, vy) 所有可能值组成的区间
(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))
------
(define z1 (make-interval 10 20))
(define z2 (make-interval 0 100))
(define z3 (sub-interval z2 z1))

(printf "~a - ~a = ~a\n" (interval->string z2) (interval->string z1) (interval->string z3))
(?== -20 (lower-bound z3))
(?== 90 (upper-bound z3))
)


;;; ex 2.9
(ex 9
(define (interval-width z)
  (/ (- (upper-bound z)
        (lower-bound z))
     2.0))
------
(define (verify fi fw)
  (define lower -100)
  (define upper 100)
  (define (random-interval)
    (make-interval (random lower upper) (random lower upper)))
  (define z1 (random-interval))
  (define z2 (random-interval))
  (define z3 (fi z1 z2))
  (printf "~a & ~a => ~a\n" (interval->string z1) (interval->string z2) (interval->string z3))
  (?== (fw (interval-width z1) (interval-width z2))
       (interval-width z3)))

(verify add-interval +)
(verify sub-interval +)

(@>> "------")
(define z1 (make-interval 10 20))
(define z2 (make-interval 20 30))
(define z3 (make-interval 20 100))
(define z3*1 (mul-interval z1 z3))
(define z3*2 (mul-interval z2 z3))
(define z3/1 (div-interval z3 z1))
(define z3/2 (div-interval z3 z2))

(?== (interval-width z1) (interval-width z2))
(?!= (interval-width z3*1) (interval-width z3*2))
(?!= (interval-width z3/1) (interval-width z3/2))
)


;;; ex 2.10
(ex 10
(define (cross-zero? z)
  (<= (lower-bound z) 0 (upper-bound z)))

(define (div-interval/v2 x y)
  (cond
    [(cross-zero? y) (error 'div-interval "interval cross zero")]
    [else (mul-interval
            x
            (make-interval
              (/ 1.0 (upper-bound y))
              (/ 1.0 (lower-bound y))))]))

(@update! ([div-interval div-interval/v2]))
------
(define z1 (make-interval 10 20))
(define z2 (make-interval 2 5))
(define z3 (make-interval -2 5))

(define z1/2 (div-interval z1 z2))
(?== 2.0 (lower-bound z1/2))
(?== 10.0 (upper-bound z1/2))

(@>> (@catch (div-interval z1 z3)))
)


;;; ex 2.11
(ex 11
;; https://www.math.pku.edu.cn/teachers/qiuzy/books/sicp/errata.htm
;; 中文版勘误: (63/-2) "每种情况中所需" => "除一种外其他情况所需"
;; 英文版: "only one of which requires more than two multiplications"

(define (mul-interval/v2 x y)
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

(define z1 (make-interval 10 20))
(define z2 (make-interval 2 5))
(define z3 (mul-interval/v2 z1 z2))

(?== 20 (lower-bound z3))
(?== 100 (upper-bound z3))
)


;;; ex 2.12
(ex 12
(define (make-center-percent c p)
  (make-interval
    (* c (- 1 (/ p 100.0)))
    (* c (+ 1 (/ p 100.0)))))

(define (center z)
  (/ (+ (lower-bound z) (upper-bound z)) 2.0))

(define (percent z)
  (* (/ (interval-width z) (center z)) 100.0))
------
(define z (make-center-percent 10.0 20.0))

(?== 8.0 (lower-bound z))
(?== 12.0 (upper-bound z))
(?== 10.0 (center z))
(?== 20.0 (percent z))
)


;;; ex 2.13
(ex 13
(define z1 (make-center-percent 10.0 0.01))
(define z2 (make-center-percent 20.0 0.02))
(define z3 (mul-interval z1 z2))

(?~= (center z3) (* (center z1) (center z2)))
(?~= (percent z3) (+ (percent z1) (percent z2)))

; (min z3)  = (min z1) * (min z2)
;           = c1 * (1 - p1/100) * c2 * (1 - p2/100)
;           = c1 * c2 * (1 - p1/100) * (1 - p2/100)
;           = c1 * c2 * (1 - (p1+p2)/100 + p1*p2/10000)
;          ~= c1 * c2 * (1 - (p1+p2)/100)
;
; (max z3) ~= c1 * c2 * (1 + (p1+p2)/100)
;
; c3  = ((min z3) + (max z3)) / 2
;    ~= c1 * c2
;
; p3  = ((max z3) - (min z3)) / 2 / c3 * 100
;    ~= (c1 * c2 * 2 * (p1+p2)/100) / 2 / (c1 * c2) * 100
;     = (p1+p2)/100 * 100
;     = p1 + p2
)


;;; ex 2.14
(ex 14
(define z1 (make-center-percent 20 0.02))
(define z2 (make-center-percent 10 0.01))
(define z3 (div-interval z1 z1))
(define z4 (div-interval z1 z2))

(?~= 1.0 (center z3))
(?~= 0.04 (percent z3))
(?~= 2.0 (center z4))
(?~= 0.03 (percent z4))
)


;;; ex 2.15
(ex 15
(define one (make-interval 1 1))
(define r1 (make-center-percent 10 0.1))
(define r2 (make-center-percent 20 0.2))

(define r3 (div-interval
             (mul-interval r1 r2)
             (add-interval r1 r2)))

(define r4 (div-interval
             one
             (add-interval
               (div-interval one r1)
               (div-interval one r2))))

(?~= (center r3) (center r4))
(printf "R1*R2/(R1+R2) = ~a, ~a%\n" (interval->string r3) (percent r3))
(printf "1/(1/R1+1/R2) = ~a, ~a%\n" (interval->string r4) (percent r4))
; Eva 是对的
)


;;; ex 2.16
(ex 16
;; 基本思路: 在待运算的区间中随机取值代入运算, 运算结果形成的区间即为结果区间
(define (calc-interval #:steps [steps 1000] #:times [times 10000] f . intvs)
  (define (random-value intv)
    (define lower (lower-bound intv))
    (define upper (upper-bound intv))
    (define delta (/ (- upper lower) steps))
    (exact->inexact
      (+ lower (* delta (random 0 steps)))))
  (define result-list
    (let loop ([times times] [result '()])
      (if (<= times 0)
          result
          (loop (- times 1)
                (cons (apply f (map random-value intvs))
                      result)))))
  (make-interval (apply min result-list)
                 (apply max result-list)))

(define (print-intervals fmt . intvs)
  (apply printf (cons fmt (map interval->string intvs))))

(define z1 (make-center-percent 6.8 10))
(define z2 (make-center-percent 4.7 5))
(define z3 (make-interval -2 2))

(print-intervals "~a + ~a = ~a : ~a\n" z1 z2 (calc-interval + z1 z2) (add-interval z1 z2))
(print-intervals "~a - ~a = ~a : ~a\n" z1 z2 (calc-interval - z1 z2) (sub-interval z1 z2))
(print-intervals "~a * ~a = ~a : ~a\n" z1 z2 (calc-interval * z1 z2) (mul-interval z1 z2))
(print-intervals "~a / ~a = ~a : ~a\n" z1 z2 (calc-interval / z1 z2) (div-interval z1 z2))
(print-intervals "~a ^ 2 = ~a : ~a\n" z3 (calc-interval sqr z3) (mul-interval z3 z3))

(print-intervals "A: ~a, A/A = ~a\n" z1 (calc-interval (lambda (v) (/ v v)) z1))

(define (par1 v1 v2) (/ (* v1 v2) (+ v1 v2)))
(define (par2 v1 v2) (/ 1 (+ (/ 1 v1) (/ 1 v2))))

(print-intervals "R1: ~a, R2: ~a, R1*R2/(R1+R2) = ~a\n" z1 z2 (calc-interval par1 z1 z2))
(print-intervals "R1: ~a, R2: ~a, 1/(1/R1+1/R2) = ~a\n" z1 z2 (calc-interval par2 z1 z2))
)


;;; ex 2.17
(ex 17
(define (last-pair lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) lst]
    [else (last-pair (cdr lst))]))

(?== '(34) (last-pair (list 23 72 149 34)))
)


;;; ex 2.18
(ex 18
(define (reverse lst)
  (let loop ([lst lst] [rst '()])
    (cond
      [(null? lst) rst]
      [else (loop (cdr lst) (cons (car lst) rst))])))

(?== '(25 16 9 4 1)
     (reverse '(1 4 9 16 25)))
)


;;; ex 2.19
(ex 19
(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

(define (cc amount coin-values)
  (cond
    [(= amount 0) 1]
    [(or (< amount 0) (no-more? coin-values)) 0]
    [else
     (+ (cc amount (except-first-denomination coin-values))
        (cc (- amount (first-denomination coin-values)) coin-values))]))

(define us-coins '(50 25 10 5 1))
(define us-coins-1 '(1 5 10 25 50))

(?== 292 (cc 100 us-coins))
(?== 292 (cc 100 us-coins-1))
)


;;; ex 2.20
(ex 20
(define (same-parity n . w)
  (let loop ([lst w] [rst (cons n '())])
    (cond
      [(null? lst) (reverse rst)]
      [(even? (- (car lst) n))
       (loop (cdr lst) (cons (car lst) rst))]
      [else (loop (cdr lst) rst)])))

(?== '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))
(?== '(2 4 6) (same-parity 2 3 4 5 6 7))
)


;;; ex 2.21
(ex 21
(let ()
  (define (square-list items)
    (if (null? items)
        '()
        (cons (sqr (car items))
              (square-list (cdr items)))))
  (?== '(1 4 9 16) (square-list '(1 2 3 4))))

(let ()
  (define (square-list items)
    (map sqr items))
  (?== '(1 4 9 16) (square-list '(1 2 3 4))))
)


;;; ex 2.22
(ex 22
(let ()
  (define (square-list items)
    (define (iter things answer)
      (if (null? things)
          answer
          (iter (cdr things)
                (cons (sqr (car things)) answer))))
    (iter items '()))
  ; 每次取 things 最左边的项, 平方后和放在之前 answer 的左边
  ; '() => '(1) => '(4 1) => '(9 4 1) => '(16 9 4 1)
  (?== '(16 9 4 1) (square-list '(1 2 3 4))))

(let ()
  (define (square-list items)
    (define (iter things answer)
      (if (null? things)
          answer
          (iter (cdr things)
                (cons answer (sqr (car things))))))
    (iter items '()))
  ; 每次取 things 最左边的项, 平方后和放在之前 answer 的右边, 但是平方项不是 list, cons 会形成嵌套的 pair
  ; '() => '(() . 1) => '((() . 1) . 4) => '(((() . 1) . 4) . 9) => '((((() . 1) . 4) . 9) . 16)
  (?== '((((() . 1) . 4) . 9) . 16) (square-list '(1 2 3 4))))
)


;;; ex 2.23
(ex 23
(define (for-each f lst)
  (cond
    [(null? lst) #t]
    [else
     (f (car lst))
     (for-each f (cdr lst))]))

(for-each (lambda (x) (printf "~a\n" x))
          '(57 321 88))
)


;;; ex 2.24
(ex 24
; TODO
)


;;; ex 2.25
(ex 25
(?== 7 (cadr (caddr '(1 3 (5 7) 9))))
(?== 7 (caar '((7))))

(define ((repeated f times) x)
  (if (= times 0)
      x
      ((repeated f (- times 1)) (f x))))

(?== 7 ((repeated cadr 6) '(1 (2 (3 (4 (5 (6 7))))))))
)


;;; ex 2.26
(ex 26
(define x '(1 2 3))
(define y '(4 5 6))

(?== '(1 2 3 4 5 6) (append x y))
(?== '((1 2 3) . (4 5 6)) (cons x y))
(?== '((1 2 3) (4 5 6)) (list x y))
)


;;; ex 2.27
(ex 27
(define x '((1 2) (3 4)))

(define (deep-reverse lst)
  (let loop ([lst lst] [rst '()])
    (if (null? lst)
        rst
        (let ([first (car lst)] [rest (cdr lst)])
          (if (pair? first)
              (loop rest (cons (loop first '()) rst))
              (loop rest (cons first rst)))))))

(?== '((3 4) (1 2)) (reverse x))
(?== '((4 3) (2 1)) (deep-reverse x))
)


;;; ex 2.28
(ex 28
(define (fringe/rec x)
  (cond
    [(null? x) '()]
    [(not (pair? x)) (cons x '())]
    [else (append (fringe (car x))
                  (fringe (cdr x)))]))

(define (fringe x)
  (define (helper tree)
    (let loop ([tree tree] [rst '()])
      (if (null? tree)
          rst
          (let ([first (car tree)] [rest (cdr tree)])
            (if (pair? first)
                (loop rest (append (helper first) rst))
                (loop rest (append (list first) rst)))))))
  (reverse (helper x)))

(define x '((1 2) (3 4)))

(?== '(1 2 3 4) (fringe x))
(?== '(1 2 3 4 1 2 3 4) (fringe (list x x)))
)


;;; ex 2.29
(ex 29
(define (make-mobile left right) (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (make-branch length structure) (list length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (mobile? structure) (pair? structure))

(define (total-weight mobile)
  (if (mobile? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

(define (balance? mobile)
  (if (mobile? mobile)
      (let* ([lb (left-branch mobile)] [rb (right-branch mobile)]
             [lbs (branch-structure lb)] [rbs (branch-structure rb)])
        (and (= (* (branch-length lb) (total-weight lbs))
                (* (branch-length rb) (total-weight rbs)))
             (balance? lbs)
             (balance? rbs)))
      #t))


(define (verify)
  (define m1 (make-mobile (make-branch 10 40) (make-branch 40 10)))
  (define m2 (make-mobile (make-branch 10 30) (make-branch 20 20)))
  (define m3 (make-mobile (make-branch 10 m1) (make-branch 10 m2)))

  (?== 50 (total-weight m1))
  (?== 50 (total-weight m2))
  (?== 100 (total-weight m3))

  (?true (balance? m1))
  (?false (balance? m2))
  (?false (balance? m3)))

(verify)

(define (make-mobile/v2 left right) (cons left right))
(define (left-branch/v2 mobile) (car mobile))
(define (right-branch/v2 mobile) (cdr mobile))

(define (make-branch/v2 length structure) (cons length structure))
(define (branch-length/v2 branch) (car branch))
(define (branch-structure/v2 branch) (cdr branch))

(@update ([make-mobile make-mobile/v2]
          [left-branch left-branch/v2]
          [right-branch right-branch/v2]
          [make-branch make-branch/v2]
          [branch-length branch-length/v2]
          [branch-structure branch-structure/v2])
  (@>> "------")
  (verify))
)


;;; ex 2.30
(ex 30
(define (square-tree tree)
  (map (lambda (e)
         (if (pair? e)
             (square-tree e)
             (sqr e)))
       tree))

(define (square-tree/v2 tree)
  (let loop ([tree tree] [rst '()])
    (if (null? tree)
        (reverse rst)
        (let ([first (car tree)] [rest (cdr tree)])
          (if (pair? first)
              (loop rest (cons (loop first '()) rst))
              (loop rest (cons (sqr first) rst)))))))

(define t1 '(1 (2 (3 4) 5)))
(define t2 '(1 (4 (9 16) 25)))

(?== t2 (square-tree t1))
(?== t2 (square-tree/v2 t1))
)


;;; ex 2.31
(ex 31
(define (tree-map f tree)
  (map (lambda (e)
         (if (pair? e)
             (tree-map f e)
             (f e)))
       tree))

(define (square-tree tree) (tree-map sqr tree))

(define t1 '(1 (2 (3 4) 5)))
(define t2 '(1 (4 (9 16) 25)))

(?== t2 (square-tree t1))
)


;;; ex 2.32
(ex 32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (e) (cons (car s) e)) rest)))))

(?== '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
     (subsets '(1 2 3)))
)


;;; ex 2.33
(ex 33
(define accumulate foldr)
------
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define l1 '(1 2 3 4 5))
(define l2 '(6 7 8 9 10))

(?== '(1 4 9 16 25) (map sqr l1))
(?== '(1 2 3 4 5 6 7 8 9 10) (append l1 l2))
(?== 5 (length l1))
)


;;; ex 2.34
(ex 34
(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* higher-terms x)))
    0
    coefficient-sequence))

; 1 + 3x + 5x^3 + x^5
(define cs '(1 3 0 5 0 1))
(?== 79 (horner-eval 2 cs))
)


;;; ex 2.35
(ex 35
(define (count-leaves tree)
  (accumulate
    +
    0
    (map (lambda (e)
           (if (pair? e)
               (count-leaves e)
               1))
         tree)))

(?== 0 (count-leaves '()))
(?== 3 (count-leaves '(1 2 3)))
(?== 8 (count-leaves '(1 (2 3) (4) 5 (6 7 8))))
)


;;; ex 2.36
(ex 36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
------
(define ss '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(?== '(22 26 30) (accumulate-n + 0 ss))
)


;;; ex 2.37
(ex 37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define v1 '(1 2 3 4))
(define v2 '(4 5 6 6))
(define v3 '(6 7 8 9))
(define m (list v1 v2 v3))
(define n '((1 4 6) (2 5 7) (3 6 8) (4 6 9)))

(?== 56 (dot-product v1 v2))
(?== '(30 56 80) (matrix-*-vector m v1))
(?== n (transpose m))
(?== '((53 64 75 82) (64 78 92 101) (75 92 109 120) (82 101 120 133))
     (matrix-*-matrix n m))
)


;;; ex 2.38
(ex 38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
------
(define lst '(1 2 3))

(?== 3/2 (fold-right / 1 lst))
(?== 3/2 (foldr / 1 lst))

(?== 1/6 (fold-left / 1 lst))
(?== 3/2 (foldl / 1 lst))   ;; fold-left != foldl

(?== '(1 (2 (3 ()))) (fold-right list '() lst))
(?== '(1 (2 (3 ()))) (foldr list '() lst))

(?== '(((() 1) 2) 3) (fold-left list '() lst))
(?== '(3 (2 (1 ()))) (foldl list '() lst))
)


;;; ex 2.39
(ex 39
(let ()
  (define (reverse sequence)
    (fold-right
      (lambda (x y) (append y (cons x '())))
      '()
      sequence))
  (?== '(4 3 2 1) (reverse '(1 2 3 4))))

(let ()
  (define (reverse sequence)
    (fold-left
      (lambda (x y) (cons y x))
      '()
      sequence))
  (?== '(4 3 2 1) (reverse '(1 2 3 4))))
)


;;; ex 2.40
(ex 40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (int/list 1 (- i 1))))
    (int/list 1 n)))
------
(?== '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))
     (unique-pairs 4))

(define (prime-sum-pairs n)
  (map
    (lambda (p)
      (let ([x (car p)] [y (cadr p)])
        (list x y (+ x y))))
    (filter
      (lambda (p)
        (prime? (+ (car p) (cadr p))))
      (unique-pairs n))))

(?== '((2 1 3) (3 2 5) (4 1 5) (4 3 7))
     (prime-sum-pairs 4))
)


;;; ex 2.41
(ex 41
(define (unique-triples n)
  (flatmap
    (lambda (i)
      (map (lambda (p) (cons i p)) (unique-pairs (- i 1))))
    (int/list 1 n)))

(?== '((3 2 1) (4 2 1) (4 3 1) (4 3 2))
     (unique-triples 4))

(define (spec-sum-triples n s)
  (filter
    (lambda (t) (= s (accumulate + 0 t)))
    (unique-triples n)))

(?== '((6 5 3) (7 4 3) (7 5 2) (7 6 1))
     (spec-sum-triples 7 14))
)


;;; ex 2.42
(ex 42
(define empty-board '())

(define (safe? k positions)
  (let ([new-row (car positions)])
    (let loop ([done-rows (cdr positions)] [row-distance 1])
      (if (null? done-rows)
          #t
          (let* ([curr-row (car done-rows)] [col-distance (abs (- new-row curr-row))])
            (if (or (= 0 col-distance) (= row-distance col-distance))
                #f
                (loop (cdr done-rows) (+ row-distance 1))))))))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (int/list 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
------
(define (check-solution sln)
  (define size (length sln))
  (define (get-row col)
    (list-ref sln (- col 1)))
  (define (two-queen-safe? c1 r1 c2 r2)
    (not (or (= c1 c2)
             (= r1 r2)
             (= (abs (- c1 c2)) (abs (- r1 r2))))))
  (let loop ([pairs (unique-pairs size)])
    (if (null? pairs)
        #t
        (let* ([first (car pairs)] [rest (cdr pairs)]
               [col1 (car first)] [col2 (cadr first)]
               [row1 (get-row col1)] [row2 (get-row col2)])
          (cond
            [(two-queen-safe? col1 row1 col2 row2)
             (loop rest)]
            [else
             (printf "Clash! (~a, ~a) - (~a, ~a)\n" col1 row1 col2 row2)
             #f])))))

(define (show-solution sln)
  (define size (length sln))
  (printf "~a\n" sln)
  (for-each
    (lambda (row)
      (for-each
        (lambda (n)
          (if (= n row)
              (printf "o ")
              (printf ". ")))
        (int/list 1 size))
      (newline))
    sln))

(define solutions (queens 8))

(define sln1 (list-ref solutions (random (- (length solutions) 1))))
(show-solution sln1)

(?== 92 (length solutions))
(?true (andmap check-solution solutions))
)


;;; 2.43
(ex 43
(define (queens/v2 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (int/list 1 board-size)))))
  (queen-cols board-size))

(define (queens-cost-time f board-size)
  (define t1 (runtime))
  (f board-size)
  (define t2 (runtime))
  (- t2 t1))

; 6 ~> 83
; 7 ~> 300
; 8 ~> 2200
(define board-size 7)
(define ct1 (queens-cost-time queens board-size))
(define ct2 (queens-cost-time queens/v2 board-size))
(printf "~a / ~a = ~a\n" ct2 ct1 (exact->inexact (/ ct2 ct1)))
)

;;; ex 2.44 ~ 2.52
; 略


;;; ex 2.53
(ex 53
(@>>
  (list 'a 'b 'c)
  ; (a b c)
  (list (list 'george))
  ; ((george))
  (cdr '((x1 x2) (y1 y2)))
  ; (((y1 y2))
  (cadr '((x1 x2) (y1 y2)))
  ; (y1 y2)
  (pair? (car '(a short list)))
  ; #f
  (memq 'red '((red shoes) (blue socks)))
  ; #f
  (memq 'red '(red shoes blue socks))
  ; (red shoes blue socks)
  )
)


;;; ex 2.54
(ex 54
;; 假定 eq? 只能比较符号, 不能比较 '()
(define (equal? lst1 lst2)
  (cond
    [(and (null? lst1) (null? lst2)) #t]
    [(or (null? lst1) (null? lst2)) #f]
    [(and (pair? lst1) (pair? lst2))
     (and (equal? (car lst1) (car lst2))
          (equal? (cdr lst1) (cdr lst2)))]
    [(and (not (pair? lst1)) (not (pair? lst2)))
     (eq? lst1 lst2)]
    [else #f]))

(?true (equal? '(this is a list)
               '(this is a list)))

(?false (equal? '(this is a list)
                '(this (is a) list)))
)


;;; ex 2.55
(ex 55
(?== 'quote (car ''abracadabra))
; 'a = (quote a)
; ''a = (quote (quote a)) = '(quote a)
; (car ''a) = (car '(quote a)) = 'quote
)


;;; ex 2.56
(ex 56
)
(run-ex 56)


; (run-ex 1 ~ 42)
; (run-ex 53 ~ )