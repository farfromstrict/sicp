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
(define (deriv0 exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp)
     (if (same-variable? exp var) 1 0)]
    [(sum? exp)
     (make-sum
       (deriv (addend exp) var)
       (deriv (augend exp) var))]
    [(product? exp)
     (make-sum
       (make-product
         (multiplier exp)
         (deriv (multiplicand exp) var))
       (make-product
         (deriv (multiplier exp) var)
         (multiplicand exp)))]
    [(exponentiation? exp)
     (let ([u (base exp)] [n (exponent exp)])
       (make-product
         (make-product n (make-exponentiation u (make-sum n -1)))
         (deriv u var)))]
    [else (error 'deriv "unknown expression type: ~a" exp)]))

(define (deriv exp var) (deriv0 exp var))

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))
(define (make-sum a1 a2)
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [else (list '+ a1 a2)]))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? '* (car x))))
(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list '* m1 m2)]))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? '** (car x))))
(define (make-exponentiation b e)
  (cond
    [(=number? e 0) 1]
    [(=number? e 1) b]
    [(=number? b 0) 0]
    [(=number? b 1) 1]
    [(and (number? b) (number? e)) (expt b e)]
    [else (list '** b e)]))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
------
; (x + 3)' = 1
(?== 1 (deriv '(+ x 3) 'x))
; (xy)' = y
(?== 'y (deriv '(* x y) 'x))
; (xy * (x + 3))' = (2xy + 3y)
(?== '(+ (* x y) (* y (+ x 3)))
     (deriv '(* (* x y) (+ x 3)) 'x))

; (x^3)' = (3x^2)
(?== '(* 3 (** x 2)) (deriv '(** x 3) 'x))
; ((xy+3)^y)' = (y * y * (xy+3)^(y-1))
(?== '(* (* y (** (+ (* x y) 3) (+ y -1))) y)
     (deriv '(** (+ (* x y) 3) y) 'x))
)


;;; ex 2.57
(ex 57
(define (not-number? exp) (not (number? exp)))

(define (make-sum/v2 . args)
  (let* ([nums-sum (apply + (filter number? args))]
         [vars (filter not-number? args)]
         [vars-count (length vars)])
    (cond
      [(= vars-count 0) nums-sum]
      [(= nums-sum 0)
       (cond
         [(= vars-count 1) (car vars)]
         [else (cons '+ vars)])]
      [else (cons '+ (cons nums-sum vars))])))
(define (addend/v2 s) (cadr s))
(define (augend/v2 s)
  (apply make-sum/v2 (cddr s)))

(define (make-product/v2 . args)
  (let* ([nums-product (apply * (filter number? args))]
         [vars (filter not-number? args)]
         [vars-count (length vars)])
    (cond
      [(= vars-count 0) nums-product]
      [(= nums-product 0) 0]
      [(= nums-product 1)
       (cond
         [(= vars-count 1) (car vars)]
         [else (cons '* vars)])]
      [else (cons '* (cons nums-product vars))])))
(define (multiplier/v2 p) (cadr p))
(define (multiplicand/v2 p)
  (apply make-product/v2 (cddr p)))

(define (deriv0/v2 exp var)
  (cond
    [(exponentiation? exp)
     (let ([u (base exp)] [n (exponent exp)])
       (make-product/v2
         n
         (make-exponentiation u (make-sum/v2 n -1))
         (deriv u var)))]
    [else (deriv0 exp var)]))

(define (deriv/v2 exp var) (deriv0/v2 exp var))
------
(@update ([make-sum make-sum/v2]
          [addend addend/v2]
          [augend augend/v2]
          [make-product make-product/v2]
          [multiplier multiplier/v2]
          [multiplicand multiplicand/v2]
          [deriv deriv/v2])
  ; (x * y * (x+3))' = 2xy + 3y
  (?== '(+ (* x y) (* y (+ x 3)))
       (deriv '(* x y (+ x 3)) 'x))
  ; (x * y * (x+y+3))' = 2xy + y*y + 3y
  (?== '(+ (* x y) (* y (+ x y 3)))
       (deriv '(* x y (+ x y 3)) 'x))
  ; ((xy+3)^y)' = (y * y * (xy+3)^(y-1))
  (?== '(* y (** (+ (* x y) 3) (+ -1 y)) y)
       (deriv '(** (+ (* x y) 3) y) 'x)))
)


;;; ex 2.58
(ex 58
; 直接实现 (b) 要求, 忽略异常输入
(define (before-member mem lst)
  (cond
    [(or (null? lst) (eq? mem (car lst))) '()]
    [else (cons (car lst) (before-member mem (cdr lst)))]))

(define (after-member mem lst)
  (cdr (member mem lst)))

(define (unpack lst)
  (cond
    [(null? lst) lst]
    [(not (pair? lst)) lst]
    [(null? (cdr lst)) (car lst)]
    [else lst]))

(define (join-with op lst)
  (cond
    [(null? lst) lst]
    [(null? (cdr lst)) lst]
    [else (cons (car lst) (cons op (join-with op (cdr lst))))]))

(define (sum?/v3 exp)
  (and (pair? exp) (member '+ exp)))
; TODO, reduce
(define (make-sum/v3 . args)
  (let* ([nums-sum (apply + (filter number? args))]
         [vars (filter not-number? args)]
         [vars-count (length vars)])
    (cond
      [(= vars-count 0) nums-sum]
      [(= nums-sum 0)
       (cond
         [(= vars-count 1) (car vars)]
         [else (join-with '+ vars)])]
      [else (join-with '+ (cons nums-sum vars))])))
(define (addend/v3 exp)
  (unpack (before-member '+ exp)))
(define (augend/v3 exp)
  (unpack (after-member '+ exp)))

(define (product?/v3 exp)
  (and (pair? exp)
       (not (sum?/v3 exp))
       (member '* exp)))
; TODO, reduce
(define (make-product/v3 . args)
  (let* ([nums-product (apply * (filter number? args))]
         [vars (filter not-number? args)]
         [vars-count (length vars)])
    (cond
      [(= nums-product 0) 0]
      [(= vars-count 0) nums-product]
      [(= nums-product 1)
       (cond
         [(= vars-count 1) (car vars)]
         [else (join-with '* vars)])]
      [else (join-with '* (cons nums-product vars))])))
(define (multiplier/v3 exp)
  (unpack (before-member '* exp)))
(define (multiplicand/v3 exp)
  (unpack (after-member '* exp)))

(define (exponentiation?/v3 exp)
  (and (pair? exp)
       (= 3 (length exp))
       (eq? '** (cadr exp))))
(define (make-exponentiation/v3 b e)
  (cond
    [(=number? e 0) 1]
    [(=number? e 1) b]
    [(=number? b 0) 0]
    [(=number? b 1) 1]
    [(and (number? b) (number? e)) (expt b e)]
    [else (list b '** e)]))
(define (base/v3 exp) (car exp))
(define (exponent/v3 exp) (caddr exp))

(define deriv/v3 deriv/v2)

(@update ([sum? sum?/v3]
          [make-sum make-sum/v3]
          [addend addend/v3]
          [augend augend/v3]
          [product? product?/v3]
          [make-product make-product/v3]
          [multiplier multiplier/v3]
          [multiplicand multiplicand/v3]
          [exponentiation? exponentiation?/v3]
          [make-exponentiation make-exponentiation/v3]
          [base base/v3]
          [exponent exponent/v3])
  ; (x * y * (x+3))' = 2xy + 3y
  (?== '((x * y) + (y * (x + 3)))
       (deriv '(x * y * (x + 3)) 'x))
  ; (x * y * (x+y+3))' = 2xy + y*y + 3y
  (?== '((x * y) + (y * (x + y + 3)))
       (deriv '(x * y * (x + y + 3)) 'x))
  ; ((xy+3)^y)' = (y * y * (xy+3)^(y-1))
  (?== '((y * ((x * y + 3) ** (-1 + y))) * y)
       (deriv '((x * y + 3) ** y) 'x))
  ; (x + 3*(x+y+2))' = 4
  (?== 4
       (deriv '(x + 3 * (x + y + 2)) 'x))
  ; ((x+3) * (x+y+2))' = 2x+y+5
  (?== '((x + 3) + (x + y + 2))
       (deriv '((x + 3) * (x + y + 2)) 'x)))
)


;;; ex 2.59
(ex 59
(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [(element-of-set? (car set1) set2)
     (cons (car set1) (union-set (cdr set1) (cdr set2)))]
    [else (cons (car set1) (union-set (cdr set1) set2))]))

(?== '(1 2 3 4 5 6 7)
     (union-set '(1 2 3 4 5) '(3 4 5 6 7)))
)


;;; ex 2.60
(ex 60
(define (element-of-set? x set)
  (cond
    [(null? set) #f]
    [(equal? x (car set)) #t]
    [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond
    [(or (null? set1) (null? set2)) '()]
    [(element-of-set? (car set1) set2)
     (cons (car set1) (intersection-set (cdr set1) set2))]
    [else (intersection-set (cdr set1) set2)]))

(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [else (union-set (cdr set1) (cons (car set1) set2))]))

(define s1 '(2 3 2 1 3 2 2))
(define s2 '(2 3 4 5))

(?true (element-of-set? 1 s1))
(?true (element-of-set? 3 s1))
(?false (element-of-set? 4 s1))

(?== '(1 2 3 2 1 3 2 2) (adjoin-set 1 s1))
(?== '(4 2 3 2 1 3 2 2) (adjoin-set 4 s1))

(?== '(2 3 2 3 2 2) (intersection-set s1 s2))
(?== '(2 2 3 1 2 3 2 2 3 4 5) (union-set s1 s2))
; 插入合并操作多时选择有重复实现, 查找操作多时选择无重复实现
)


;;; ex 2.61
(ex 61
(define (adjoin-set x set)
  (cond
    [(null? set) (cons x '())]
    [else
     (let ([first (car set)] [rest (cdr set)])
       (cond
         [(< x first) (cons x set)]
         [(> x first) (cons first (adjoin-set x rest))]
         [else set]))]))

(?== '(10) (adjoin-set 10 '()))
(?== '(1 3 5 7 10) (adjoin-set 10 '(1 3 5 7)))
(?== '(1 3 5 7) (adjoin-set 3 '(1 3 5 7)))
(?== '(1 3 4 5 7) (adjoin-set 4 '(1 3 5 7)))
)


;;; ex 2.62
(ex 62
(define (intersection-set set1 set2)
  (cond
    [(or (null? set1) (null? set2)) '()]
    [else
     (let ([f1 (car set1)] [f2 (car set2)])
       (cond
         [(< f1 f2) (intersection-set (cdr set1) set2)]
         [(> f1 f2) (intersection-set set1 (cdr set2))]
         [else (cons f1 (intersection-set (cdr set1) (cdr set2)))]))]))

(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [else
     (let ([f1 (car set1)] [f2 (car set2)])
       (cond
         [(< f1 f2) (cons f1 (union-set (cdr set1) set2))]
         [(> f1 f2) (cons f2 (union-set set1 (cdr set2)))]
         [else (cons f1 (union-set (cdr set1) (cdr set2)))]))]))
------
(?== '(1 2 3 4 5 6 7 8) (union-set '(1 3 5 7) '(2 4 6 8)))
(?== '(1 3 5 7 9 11) (union-set '(1 3 5 7) '(1 5 9 11)))
)


;;; ex 2.63
(ex 63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

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
------
(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(?== '(1 3 5 7 9 11) (tree->list-1 t1))
(?== '(1 3 5 7 9 11) (tree->list-2 t1))
(?== '(1 3 5 7 9 11) (tree->list-1 t2))
(?== '(1 3 5 7 9 11) (tree->list-2 t2))
(?== '(1 3 5 7 9 11) (tree->list-1 t3))
(?== '(1 3 5 7 9 11) (tree->list-2 t3))

; 两个过程结果相同
; tree->list-1 增长速度量级为 O(N) = N*logN
; tree->list-2 增长速度量级为 O(N) = N
)


;;; ex 2.64
(ex 64
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (list->tree elements)
  (car (patial-tree elements (length elements))))

(define (patial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ([left-size (quotient (- n 1) 2)]
             [left-result (patial-tree elts left-size)]
             [left-tree (car left-result)]
             [non-left-elts (cdr left-result)]
             [right-size (- n (+ left-size 1))]
             [this-entry (car non-left-elts)]
             [right-result (patial-tree (cdr non-left-elts) right-size)]
             [rigth-tree (car right-result)]
             [remaining-elts (cdr right-result)])
        (cons (make-tree this-entry left-tree rigth-tree)
              remaining-elts))))
------
(?== '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
     (list->tree '(1 3 5 7 9 11)))

; 二分法将列表划分为两部分
; 先处理左半部分, 得到结果 (cons 左半树 剩余元素)
; 剩余元素 = 根元素 + 右半部分元素
; 再处理右半部分元素, 得到结果 (cons 右半树 空表)
; 组合 根元素, 左半树, 右半树 得到最终结果
;
; 使用二分法处理, O(N)=log(N)
)


;;; ex 2.65
(ex 65
(define tree->list tree->list-2)

(define (intersection-set/tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1) (tree->list tree2))))

(define (union-set/tree tree1 tree2)
  (list->tree (union-set (tree->list tree1) (tree->list tree2))))

(define t1 '(3 (1 () ()) (5 () (7 () ()))))
(define t2 '(4 (2 () ()) (6 () (8 () ()))))
(define t3 '(5 (1 () ()) (9 () (11 () ()))))

(?== '()
     (intersection-set/tree t1 t2))
(?== '(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () (8 () ()))))
     (union-set/tree t1 t2))
(?== '(1 () (5 () ()))
     (intersection-set/tree t1 t3))
(?== '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
     (union-set/tree t1 t3))

; list->tree, tree->list 的时间复杂度为 O(N) = log(N)
; intersection-set, union-set 的时间复杂度为 O(N) = N
; 所以总的时间复杂度为 O(N) = N + log(N) = N
)


;;; ex 2.66
(ex 66
(define (make-record key value) (list key value))
(define (key record) (car record))
(define (value record) (cadr record))

(define (lookup given-key set-of-records)
  (cond
    [(null? set-of-records) #f]
    [else
     (let* ([root (entry set-of-records)] [root-key (key root)])
       (cond
         [(= given-key root-key) root]
         [(< given-key root-key) (lookup given-key (left-branch set-of-records))]
         [else (lookup given-key (right-branch set-of-records))]))]))

(define s1 '((5 50) ((2 20) ((1 10) () ()) ((3 30) () ())) ((7 70) ((6 60) () ()) ((8 80) () ()))))

(?== '(3 30) (lookup 3 s1))
(?== #f (lookup 4 s1))
(?== '(7 70) (lookup 7 s1))
(?== #f (lookup 10 s1))
)


;;; ex 2.67
(ex 67
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? 'leaf (car object)))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (helper bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (helper (cdr bits) tree))
              (helper (cdr bits) next-branch)))))
  (helper bits tree))

(define (choose-branch bit tree)
  (cond
    [(= bit 0) (car tree)]
    [(= bit 1) (cadr tree)]
    [else (error 'choose-branch "bad bit: ~a" bit)]))

(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))
; A - 0
; B - 10
; C - 111
; D - 110
------
(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(?== '(A D A B B C A) (decode sample-message sample-tree))
)


;;; ex 2.68
(ex 68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let loop ([branch tree] [rst '()])
    (if (leaf? branch)
        (if (eq? symbol (symbol-leaf branch))
            (reverse rst)
            (error 'encode-symbol "bad symbol: ~a" symbol))
        (if (member symbol (symbols branch))
            (let ([left-branch (car branch)] [right-branch (cadr branch)])
              (cond
                [(member symbol (symbols left-branch))
                 (loop left-branch (cons 0 rst))]
                [else (loop right-branch (cons 1 rst))]))
            (error 'encode-symbol "bad symbol: ~a" symbol)))))
------
(?== '(0 1 1 0 0 1 0 1 0 1 1 1 0)
     (encode '(A D A B B C A) sample-tree))
)


;;; ex 2.69
(ex 69
(define (adjoin-set x set)
  (cond
    [(null? set) (list x)]
    [(< (weight x) (weight (car set))) (cons x set)]
    [else (cons (car set) (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= 1 (length set))
      (car set)
      (successive-merge
        (adjoin-set
          (make-code-tree (car set) (cadr set))
          (cddr set)))))
------
(define pairs '((A 4) (B 2) (C 1) (D 1)))
(define tree (generate-huffman-tree pairs))

(?== '(0) (encode '(A) tree))
(?== '(1 0) (encode '(B) tree))
(?== '(1 1 1) (encode '(C) tree))
(?== '(1 1 0) (encode '(D) tree))
)


;;; ex 2.70
(ex 70
(define pairs '((a 2) (na 16) (boom 1) (Sha 3) (Get 2) (yip 9) (job 2) (Wah 1)))

(define song
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip 
    yip yip yip yip yip
    Sha boom))

(define htree (generate-huffman-tree pairs))

(define code (encode song htree))
(?== song (decode code htree))
(printf "Word count: ~a, Code bit count: ~a\n" (length song) (length code))
; 霍夫曼编码需要 84 个二进制位, 如果是定长编码, 需要二进制位 3 * 36 = 108
)


;;; ex 2.71
(ex 71
; n = 5
(define pairs '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define htree (generate-huffman-tree pairs))
(@>> htree)
(?== '(1) (encode '(E) htree))
(?== '(0 0 0 0) (encode '(A) htree))
; 最频繁的符号用 1 个二进制位, 最不频繁的符号用 (n-1) 个二进制位
)


;;; ex 2.72
(ex 72
; (((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31)
;
; 考虑如 2.71 描述的特殊情况:
; 最频繁的符号: 符号出现在符号表最右侧, member 查找的时间复杂度为 O(N), 查找次数为 O(1), 总复杂度为 O(N) * O(1) = O(N)
; 最不频繁的符号: 符号出现在符号表最左侧, member 查找的时间复杂度为 O(1), 查找次数为 O(N), 总复杂度为 O(1) * O(N) = O(N)
; 最差情况: 符号出现在符号表中间, member 查找的时间复杂度为 O(N), 查找次数为 O(N), 总复杂度为 O(N) * O(N) = O(N^2)
;
; 一般情况: member 查找的时间复杂度为 O(N), 查找次数为 O(logN), 总复杂度为 O(N*logN)
)


;;; ex 2.73
(ex 73
(define table (mcons '**rules** '()))

(define (massoc key mlst)
  (if (null? mlst)
      #f
      (let ([first (mcar mlst)])
        (if (equal? key (mcar first))
            first
            (massoc key (mcdr mlst))))))

(define (madd element mlst)
  (if (null? (mcdr mlst))
      (set-cdr! mlst (mcons element '()))
      (set-cdr! mlst (mcons element (mcdr mlst)))))

(define (put tag type proc)
  (let ([records (massoc tag (mcdr table))]
        [record (mcons type proc)])
    (if records
        (madd record records)
        (madd (mcons tag (mcons record '())) table))))

(define (get tag type)
  (let ([records (massoc tag (mcdr table))])
    (if records
        (let ([record (massoc type (mcdr records))])
          (if record
              (mcdr record)
              #f))
        #f)))
------
(define deriv-proc #f)

(define (clear)
  (set-cdr! table '()))

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp)
     (if (same-variable? exp var) 1 0)]
    [else
     (deriv-proc exp var)]))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-sum . args)
  (let* ([nums-sum (apply + (filter number? args))]
         [vars (filter not-number? args)]
         [vars-count (length vars)])
    (cond
      [(= vars-count 0) nums-sum]
      [(= nums-sum 0)
       (cond
         [(= vars-count 1) (car vars)]
         [else (cons '+ vars)])]
      [else (cons '+ (cons nums-sum vars))])))

(define (make-product . args)
  (let* ([nums-product (apply * (filter number? args))]
         [vars (filter not-number? args)]
         [vars-count (length vars)])
    (cond
      [(= vars-count 0) nums-product]
      [(= nums-product 0) 0]
      [(= nums-product 1)
       (cond
         [(= vars-count 1) (car vars)]
         [else (cons '* vars)])]
      [else (cons '* (cons nums-product vars))])))

(define (make-exponentiation b e)
  (list '** b e))

(define (deriv/sum args var)
  (apply make-sum (map (lambda (arg) (deriv arg var)) args)))

(define (deriv/product args var)
  (let ([v1 (car args)] [v2 (apply make-product (cdr args))])
    (make-sum
      (make-product v1 (deriv v2 var))
      (make-product (deriv v1 var) v2))))

(define (deriv/exponentiation args var)
  (let ([u (car args)] [n (cadr args)])
    (make-product
      n
      (make-exponentiation u (make-sum n -1))
      (deriv u var))))

(define (deriv-proc/v1 exp var)
  ((get 'deriv (operator exp)) (operands exp) var))

(define (deriv-proc/v2 exp var)
  ((get (operator exp) 'deriv) (operands exp) var))

(define (verify)
  (?== '(+ (* x y) (* y (+ x 3)))
       (deriv '(* x y (+ x 3)) 'x))
  (?== '(+ (* x y) (* y (+ x y 3)))
       (deriv '(* x y (+ x y 3)) 'x))
  (?== '(* y (** (+ (* x y) 3) (+ -1 y)) y)
       (deriv '(** (+ (* x y) 3) y) 'x))
  (?== 4
       (deriv '(+ x (* 3 (+ x y 2))) 'x))
  (?== '(+ (+ x 3) (+ x y 2))
       (deriv '(* (+ x 3) (+ x y 2)) 'x)))

(@update ([deriv-proc deriv-proc/v1])
  (clear)
  (put 'deriv '+ deriv/sum)
  (put 'deriv '* deriv/product)
  (put 'deriv '** deriv/exponentiation)
  (verify))

(@>> "------")

(@update ([deriv-proc deriv-proc/v2])
  (clear)
  (put '+ 'deriv deriv/sum)
  (put '* 'deriv deriv/product)
  (put '** 'deriv deriv/exponentiation)
  (verify))

; 获取 exp 表达式的运算符, 根据运算符获取到对应的求导规则进行求导
; 纯数值和变量无运算符, 无法从规则表中获取规则, 也就无法进行数据导向分派
)


;;; ex 2.74
(ex 74
; get 过程从 set 中获取指定 tag 的元素
(define (get tag set) #f)

; a) 雇员文件结构: 以雇员名字为 key 的集合
;    分支机构应提供从文件中获取 record 的方法
(define get-record-methods '())
(define (get-record employee-name file)
  ((get 'get-record get-record-methods) employee-name file))

; b) 提供从 record 中获取 salary 的方法
(define get-salary-methods '())
(define (get-salary employee-name file)
  ((get 'get-salary get-salary-methods)
   (get-record employee-name file)))

; c)
(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      #f
      (let* ([file (car division-files)]
             [record (get-record employee-name file)])
        (if record
            record
            (find-employee-record employee-name (cdr division-files))))))

; d) 将新公司的雇员文件添加到 division-files 中
;    将新公司的相关方法添加到 get-record-methods, get-salary-methods 等方法列表中
)


;;; ex 2.75
(ex 75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond
      [(eq? op 'real-part) (* r (cos a))]
      [(eq? op 'imag-part) (* r (sin a))]
      [(eq? op 'magnitude) r]
      [(eq? op 'angle) a]
      [else (error 'make-from-mag-ang "unknown operation: ~a" op)]))
  dispatch)

(define z (make-from-mag-ang 10.0 (/ pi 6)))
(?~= 8.66 (z 'real-part))
(?~= 5.0 (z 'imag-part))
(?~= 10.0 (z 'magnitude))
(?~= 0.523 (z 'angle))
)


;;; ex 2.76
(ex 76
; 1) 带有显式分派的通用型操作: 新类型-新增分派类型, 新操作-新增操作过程
; 2) 数据导向 (一级:类型, 二级:操作): 新类型-新增类型及现有操作, 新操作-所有类型新增操作
; 3) 消息传递 (一级:操作, 二级:类型): 新类型-所有操作新增类型, 新操作-新增操作及现有类型
;
; 如果新增类型常见, 数据导向方式合适
; 如果新增操作常见, 消息传递方式合适
)


;;; ex 2.77
(ex 77
(@update! ([table (mcons '**numbers** '())]))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag "bad tagged datum, ~a" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error 'contents "bad tagged datum, ~a" datum)))

(define (apply-generic op . args)
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (error 'apply-generic "no method for this types, ~a" op))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cond
        [(> d 0) (cons (/ n g) (/ d g))]
        [(< d 0) (cons (- (/ n g)) (- (/ d g)))]
        [else (error 'make-rat "denom is zero")])))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

(define (install-rectangular-package)
  (define (make-from-real-imag x y) (cons x y))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag z) (attach-tag 'rectangular z))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (make-from-mag-ang r a) (cons r a))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))

  (define (tag z) (attach-tag 'polar z))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (format-real-imag c)
  (format "(~a, ~a)" (real-part c) (imag-part c)))

(define (install-number-packages)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))
------
(install-number-packages)

(define z (make-from-real-imag 3 4))
(?== 5 (magnitude z))

; apply-generic 被调用 2 次
;
; z = '(complex rectangular 3 . 4)
; (magnitude '(complex rectangular 3 . 4))
;   = (apply-generic 'magnitude '(complex rectangular 3 . 4))
;   = ((get 'magnitude '(complex)) ca)
;   = (magnitude '(rectangular 3 . 4))  ;; 此 magnitude 过程在 complex 包中被装载 (递归)
;   = (apply-generic 'magnitude '(rectangular 3 . 4))
;   = ((get 'magnitude '(rectangular)) (3 . 4))
;   = (magnitude (3 . 4))  ;; 此 magnitude 过程在 rectangular 包中被装载
;   = (sqrt (+ (sqr 3) (sqr 4)))
;   = 5
)


;;; ex 2.78
(ex 78
(define (attach-tag/v2 type-tag contents)
  (cond
    [(or (number? contents) (symbol? contents)) contents]
    [else (cons type-tag contents)]))

(define (type-tag/v2 datum)
  (if (pair? datum) (car datum) 'scheme-number))

(define (contents/v2 datum)
  (if (pair? datum) (cdr datum) datum))

(@update! ([attach-tag attach-tag/v2]
           [type-tag type-tag/v2]
           [contents contents/v2]))
------
(install-number-packages)

(define x (make-scheme-number 10))
(define y (make-scheme-number 20))
(printf "~a + ~a = ~a\n" x y (add x y))
(printf "~a * ~a = ~a\n" x y (mul x y))

(define c1 (make-from-real-imag 3 4))
(define c2 (make-from-real-imag 5 6))
(printf "~a + ~a = ~a\n" (format-real-imag c1)
                         (format-real-imag c2)
                         (format-real-imag (add c1 c2)))
(printf "~a * ~a = ~a\n" (format-real-imag c1)
                         (format-real-imag c2)
                         (format-real-imag (mul c1 c2)))

)


;;; ex 2.79
(ex 79
(define (install-equ-to-packges)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (nx ny) (= nx ny)))
  (put 'equ? '(scheme-number rational)
       (lambda (n r) (= (car r) (* n (cdr r)))))
  (put 'equ? '(scheme-number complex)
       (lambda (n c) (and (= 0 (imag-part c))
                          (= n (real-part c)))))
  (put 'equ? '(rational scheme-number)
       (lambda (r n) (= (car r) (* n (cdr r)))))
  (put 'equ? '(rational rational)
       (lambda (rx ry) (= (* (car rx) (cdr ry))
                          (* (cdr rx) (car ry)))))
  (put 'equ? '(rational complex)
       (lambda (r c) (and (= 0 (imag-part c))
                          (= (car r) (* (cdr r) (real-part c))))))
  (put 'equ? '(complex scheme-number)
       (lambda (c n) (and (= 0 (imag-part c))
                          (= n (real-part c)))))
  (put 'equ? '(complex rational)
       (lambda (c r) (and (= 0 (imag-part c))
                          (= (car r) (* (cdr r) (real-part c))))))
  (put 'equ? '(complex complex)
       (lambda (cx cy) (and (= (real-part cx) (real-part cy))
                            (= (imag-part cx) (imag-part cy)))))
  )

(define (equ? x y) (apply-generic 'equ? x y))
------
(install-number-packages)
(install-equ-to-packges)

(define n1 (make-scheme-number 10))
(define n2 (make-scheme-number 20))
(define r1 (make-rational 10 1))
(define r2 (make-rational 20 2))
(define r3 (make-rational 20 3))
(define c1 (make-from-real-imag 10 10))
(define c2 (make-from-real-imag 10 10))
(define c3 (make-from-real-imag 10 0))

(?true (equ? n1 n1))
(?false (equ? n1 n2))

(?true (equ? r1 r2))
(?false (equ? r1 r3))

(?true (equ? n1 r1))
(?true (equ? n1 r2))
(?false (equ? n2 r1))

(?true (equ? r1 n1))
(?true (equ? r2 n1))
(?false (equ? r1 n2))

(?true (equ? c1 c2))
(?false (equ? c1 c3))

(?true (equ? c3 n1))
(?true (equ? n1 c3))
(?false (equ? c1 n1))
(?false (equ? n1 c1))

(?true (equ? c3 r1))
(?true (equ? r1 c3))
(?false (equ? c1 r3))
(?false (equ? r3 c1))
)


;;; ex 2.80
(ex 80
(define (install-=zero?-to-packages)
  (put '=zero? '(scheme-number)
    (lambda (n) (= 0 n)))
  (put '=zero? '(rational)
    (lambda (r) (= 0 (car r))))
  (put '=zero? '(complex)
    (lambda (c) (and (= 0 (real-part c))
                     (= 0 (imag-part c)))))
  )
(define (=zero? x) (apply-generic '=zero? x))
------
(install-number-packages)
(install-=zero?-to-packages)

(define n1 (make-scheme-number 0))
(define n2 (make-scheme-number 10))
(define r1 (make-rational 0 3))
(define r2 (make-rational 10 3))
(define c1 (make-from-real-imag 0 0))
(define c2 (make-from-real-imag 10 0))
(define c3 (make-from-real-imag 0 10))
(define c4 (make-from-real-imag 10 10))

(?true (=zero? n1))
(?false (=zero? n2))
(?true (=zero? r1))
(?false (=zero? r2))
(?true (=zero? c1))
(?false (=zero? c2))
(?false (=zero? c3))
(?false (=zero? c4))
)


;;; ex 2.81
(ex 81
; a) 无限递归
;    (apply-generic 'exp c1 c2)
;    =>
;    (get 'exp '(complex complex)) = #f
;    =>
;    (get-coercion 'complex 'complex) = complex->complex
;    =>
;    (apply-generic 'exp (complex->complex c1) c2)
;    =>
;    (apply-generic 'exp c1 c2)
;
; b) 不能正确工作, 有可能无限递归
;
; c) 先判断参数类型, 如果相同则不进行强转
(define get-coercion #f)

(define (apply-generic/v2 op . args)
  (define (no-method-error type-tags)
    (error 'apply-generic "No method for these types, ~a" (list op type-tags)))
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (if (= 2 (length args))
            (let ([type1 (car type-tags)]
                  [type2 (cadr type-tags)]
                  [a1 (car args)]
                  [a2 (cadr args)])
              (if (eq? type1 type2)
                  (no-method-error type-tags)
                  (let ([t1->t2 (get-coercion type1 type2)]
                        [t2->t1 (get-coercion type2 type1)])
                    (cond
                      [t1->t2 (apply-generic/v2 op (t1->t2 a1) a2)]
                      [t2->t1 (apply-generic/v2 op a1 (t2->t1 a2))]
                      [else (no-method-error type-tags)]))))
            (no-method-error type-tags)))))
)


;;; ex 2.82
(ex 82
(define get-coercion #f)

(define (apply-generic/v3 op . args)
  (define (no-method-error type-tags)
    (error 'apply-generic "No method for these types, ~a" (list op type-tags)))
  (define (same-type? type-tags)
    (let ([first (car type-tags)] [rest (cdr type-tags)])
      (andmap (lambda (e) (eq? e first)) rest)))
  (define (get-coercions one-type type-tags)
    (let ([procs (map (lambda (type-tag) (get-coercion type-tag one-type)) type-tags)])
      (if (andmap identity procs) procs #f)))
  (define (try-coercions type-tags args)
    (let loop ([ttags type-tags])
      (if (null? ttags)
          #f
          (let ([procs (get-coercions (car ttags) type-tags)])
            (if procs
                (map (lambda (proc arg) (apply proc arg)) procs args)
                (loop (cdr ttags)))))))

  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (cond
      [proc (apply proc (map contents args))]
      [(null? type-tags) (no-method-error type-tags)]
      [(same-type? type-tags) (no-method-error type-tags)]
      [else
        (let ([new-args (try-coercions type-tags args)])
          (if new-args
              (apply-generic/v3 op new-args)
              (no-method-error type-tags)))])))

; 1) 有可能存在多种强制方式, 其中方式A get 获取不到对应 method, 而方式B可以. 如果方式A先被触发, 则程序会报错而不是继续尝试方式B
; 2) 可能存在多个参数同时处理无对应 method, 参数两两分别处理有对应 method 的情况
)


;;; ex 2.83
(ex 83
(@update! ([table (mcons '**numbers** '())]))

(define types-list '(integer rational real complex))

(define (divide-zero-error^ tag) (error tag "divide by zero"))

(define (attach-tag^ type-tag contents) (cons type-tag contents))
(define (type-tag^ datum)
  (if (pair? datum)
      (car datum)
      (error 'type-tag "bad tagged datum: ~a" datum)))
(define (contents^ datum)
  (if (pair? datum)
      (cdr datum)
      (error 'contents "bad tagged datum: ~a" datum)))

(define (install-integer-package^)
  (define (tag z) (attach-tag^ 'integer z))
  (define (make z) (tag (exact-floor z)))
  (put 'make 'integer make)
  (put 'value '(integer) identity)
  (put '=0?^ '(integer)
    (lambda (z) (= z 0)))
  (put '+?^ '(integer)
    (lambda (z) (> z 0)))
  (put '-?^ '(integer)
    (lambda (z) (< z 0)))
  (put '=?^ '(integer integer)
    (lambda (zx zy) (= zx zy)))
  (put '-^ '(integer)
    (lambda (z) (tag (- 0 z))))
  (put '+^ '(integer integer)
    (lambda (zx zy) (tag (+ zx zy))))
  (put '-^ '(integer integer)
    (lambda (zx zy) (tag (- zx zy))))
  (put '*^ '(integer integer)
    (lambda (zx zy) (tag (* zx zy))))
  (put '/^ '(integer integer)
    (lambda (zx zy)
      (if (= 0 zy)
          (divide-zero-error^ 'integer)
          (if (= 0 (remainder zx zy))
              (tag (/ zx zy))
              (make-rational^ (tag zx) (tag zy))))))
  (put 'gcd '(integer integer)
    (lambda (zx zy) (tag (gcd zx zy))))
  'done)

(define (make-integer^ n) ((get 'make 'integer) n))
(define (gcd^ nx ny) (apply-generic^ 'gcd nx ny))

(define (install-rational-package^)
  (define (numer z) (car z))
  (define (denom z) (cdr z))
  (define (tag z) (attach-tag^ 'rational z))
  (define (make nn nd)
    (cond
      [(=0?^ nd) (error 'make-rational "denom is zero")]
      [(=0?^ nn) (cons (make-integer^ 0) (make-integer^ 1))]
      [else (let* ([ng (gcd^ nn nd)] [rnn (/^ nn ng)] [rnd (/^ nd ng)])
              (if (+?^ rnd)
                  (cons rnn rnd)
                  (cons (-^ rnn) (-^ rnd))))]))
  (define (reciprocal z) (make (denom z) (numer z)))
  (define (minus z) (make (-^ (numer z)) (denom z)))
  (define (add zx zy)
    (let ([zxn (numer zx)] [zxd (denom zx)] [zyn (numer zy)] [zyd (denom zy)])
      (make (+^ (*^ zxn zyd) (*^ zyn zxd))
            (*^ zxd zyd))))
  (define (sub zx zy) (add zx (minus zy)))
  (define (mul zx zy)
    (let ([zxn (numer zx)] [zxd (denom zx)] [zyn (numer zy)] [zyd (denom zy)])
      (make (*^ zxn zyn) (*^ zxd zyd))))
  (define (div zx zy) (mul zx (reciprocal zy)))
  (put 'make 'rational (lambda (nn nd) (tag (make nn nd))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'value '(rational)
    (lambda (z) (/ (value^ (numer z)) (value^ (denom z)))))
  (put '=0?^ '(rational) (lambda (z) (=0?^ (numer z))))
  (put '+?^ '(rational) (lambda (z) (+?^ (numer z))))
  (put '-?^ '(rational) (lambda (z) (-?^ (numer z))))
  (put '=?^ '(rational rational)
    (lambda (zx zy)
      (=?^ (*^ (numer zx) (denom zy))
            (*^ (denom zx) (numer zy)))))
  (put '-^ '(rational)
    (lambda (z) (tag (minus z))))
  (put 'reciprocal '(rational)
    (lambda (z) (tag (reciprocal z))))
  (put '+^ '(rational rational)
    (lambda (zx zy) (tag (add zx zy))))
  (put '-^ '(rational rational)
    (lambda (zx zy) (tag (sub zx zy))))
  (put '*^ '(rational rational)
    (lambda (zx zy) (tag (mul zx zy))))
  (put '/^ '(rational rational)
    (lambda (zx zy) (tag (div zx zy))))
  'done)

(define (make-rational^ nn nd) ((get 'make 'rational) nn nd))
(define (numer^ q) (apply-generic^ 'numer q))
(define (denom^ q) (apply-generic^ 'denom q))
(define (reciprocal^ q) (apply-generic^ 'reciprocal q))

(define (install-real-package^)
  (define delta 1e-9)
  (define (tag z) (attach-tag^ 'real z))
  (put 'make 'real tag)
  (put 'value '(real) identity)
  (put '=0?^ '(real)
    (lambda (z) (= z 0)))
  (put '+?^ '(real)
    (lambda (z) (> z 0)))
  (put '-?^ '(real)
    (lambda (z) (< z 0)))
  (put '=?^ '(real real)
    (lambda (zx zy) (< (abs (- zx zy)) delta)))
  (put '-^ '(real)
    (lambda (z) (tag (- z))))
  (put '+^ '(real real)
    (lambda (zx zy) (tag (+ zx zy))))
  (put '-^ '(real real)
    (lambda (zx zy) (tag (- zx zy))))
  (put '*^ '(real real)
    (lambda (zx zy) (tag (* zx zy))))
  (put '/^ '(real real)
    (lambda (zx zy)
      (if (= zy 0)
          (divide-zero-error^ 'real)
          (tag (/ zx zy)))))
  'done)

(define (make-real^ x) ((get 'make 'real) x))

(define (install-complex-package^)
  (define (tag z) (attach-tag^ 'complex z))
  (define (make qr qi) (cons qr qi))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (minus z)
    (make (-^ (real-part z)) (-^ (imag-part z))))
  (define (add zx zy)
    (let ([zxr (real-part zx)] [zxi (imag-part zx)]
          [zyr (real-part zy)] [zyi (imag-part zy)])
      (make (+^ zxr zyr) (+^ zxi zyi))))
  (define (sub zx zy) (add zx (minus zy)))
  (define (mul zx zy)
    (let ([zxr (real-part zx)] [zxi (imag-part zx)]
          [zyr (real-part zy)] [zyi (imag-part zy)])
      (make (-^ (*^ zxr zyr) (*^ zxi zyi))
            (+^ (*^ zxr zyi) (*^ zxi zyr)))))
  (define (div zx zy)
    (let* ([zyr (real-part zy)] [zyi (imag-part zy)]
           [mm (+^ (*^ zyr zyr) (*^ zyi zyi))]
           [nzy (make (/^ zyr mm) (-^ (/^ zyi mm)))])
      (mul zx nzy)))
  (put 'make 'complex (lambda (qr qi) (tag (make qr qi))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put '=0?^ '(complex)
    (lambda (z) (and (=0?^ (real-part z))
                     (=0?^ (imag-part z)))))
  (put '=?^ '(complex complex)
    (lambda (zx zy)
      (and (=?^ (real-part zx) (real-part zy))
           (=?^ (imag-part zx) (imag-part zy)))))
  (put '-^ '(complex)
    (lambda (z) (tag (minus z))))
  (put '+^ '(complex complex)
    (lambda (zx zy) (tag (add zx zy))))
  (put '-^ '(complex complex)
    (lambda (zx zy) (tag (sub zx zy))))
  (put '*^ '(complex complex)
    (lambda (zx zy) (tag (mul zx zy))))
  (put '/^ '(complex complex)
    (lambda (zx zy) (tag (div zx zy))))
  'done)

(define (make-complex^ qr qi) ((get 'make 'complex) qr qi))
(define (real-part^ c) (apply-generic^ 'real-part c))
(define (imag-part^ c) (apply-generic^ 'imag-part c))

(define (install-tools1-to-packages^)
  (put 'raise '(integer)
    (lambda (z) (make-rational^ (make-integer^ z) (make-integer^ 1))))
  (put 'raise '(rational)
    (lambda (z) (make-real^ (exact->inexact (value^ (make-rational^ (car z) (cdr z)))))))
  (put 'raise '(real)
    (lambda (z) (make-complex^ (make-real^ z) (make-real^ 0))))
  (put 'project '(rational)
    (lambda (z) (car z)))
  (put 'project '(real)
    (lambda (z) (make-integer^ z)))
  (put 'project '(complex)
    (lambda (z) (car z)))
  'done)

(define (no-method-error^ op type-tags)
  (error 'apply-generic "no method for these types, ~a" (list op type-tags)))

(define (apply-generic^/v1 op . args)
  (let* ([type-tags (map type-tag^ args)]
         [proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (no-method-error^ op type-tags))))

(define apply-generic^ apply-generic^/v1)

(define (raise^ x) (apply-generic^ 'raise x))
(define (project^ x) (apply-generic^ 'project x))

(define (value^ x) (apply-generic^ 'value x))
(define (=0?^ x) (apply-generic^ '=0?^ x))
(define (+?^ x) (apply-generic^ '+?^ x))
(define (-?^ x) (apply-generic^ '-?^ x))
(define (=?^ x y) (apply-generic^ '=?^ x y))
(define (+^ . args) (apply apply-generic^ (cons '+^ args)))
(define (-^ . args) (apply apply-generic^ (cons '-^ args)))
(define (*^ . args) (apply apply-generic^ (cons '*^ args)))
(define (/^ . args) (apply apply-generic^ (cons '/^ args)))

(define (install-number-packages^)
  (install-integer-package^)
  (install-rational-package^)
  (install-real-package^)
  (install-complex-package^))
------
(install-number-packages^)
(install-tools1-to-packages^)

(define n1 (make-integer^ 10))
(define n2 (make-integer^ 3))
(define q1 (make-rational^ n1 n2))
(define r1 (make-real^ 3.14159))
(@>> (raise^ n1) (raise^ q1) (raise^ r1))
)


;;; ex 2.84
(ex 84
(define (same-type?^ type-tags)
  (let ([first (car type-tags)] [rest (cdr type-tags)])
    (andmap (lambda (t) (eq? first t)) rest)))

(define (higher-type^ type1 type2)
  (let loop ([lst types-list])
    (cond
      [(null? lst) (error 'higher-type "unknown types: ~a, ~a" type1 type2)]
      [(eq? (car lst) type1) type2]
      [(eq? (car lst) type2) type1]
      [else (loop (cdr lst))])))

(define (apply-generic^/v2 op . args)
  (define (raise-to src-type dst-type arg)
    (let loop ([rst arg])
      (let ([curr-type (type-tag^ rst)])
        (if (eq? curr-type dst-type)
          rst
          (loop (raise^ rst))))))
  (define (raises type-tags args)
    (cond
      [(or (null? args) (null? (cdr args))) args]
      [else
        (let loop ([last-type (car type-tags)]
                   [done-args (cons (car args) '())]
                   [rest-types (cdr type-tags)]
                   [rest-args (cdr args)])
          (if (null? rest-args)
            done-args
            (let* ([curr-arg (car rest-args)]
                   [curr-type (car rest-types)]
                   [htype (higher-type^ last-type curr-type)]
                   [new-done-args
                     (cond
                       [(eq? htype last-type)
                        (append done-args (cons (raise-to curr-type htype curr-arg) '()))]
                       [else
                        (append (map (lambda (e) (raise-to last-type htype e)) done-args)
                                (cons curr-arg '()))])])
              (loop htype
                    new-done-args
                    (cdr rest-types)
                    (cdr rest-args)))))]))
  (let* ([type-tags (map type-tag^ args)]
         [proc (get op type-tags)])
    (cond
      [proc (apply proc (map contents args))]
      [(null? args) (no-method-error^ op type-tags)]
      [(same-type?^ type-tags) (no-method-error^ op type-tags)]
      [else (apply apply-generic^/v2 (cons op (raises type-tags args)))])))

(@update! ([apply-generic^ apply-generic^/v2]))
------
(install-number-packages^)
(install-tools1-to-packages^)

(define n1 (make-integer^ 10))
(define n2 (make-integer^ 3))

(define q1 (make-rational^ n1 n2))
(define r1 (make-real^ 3.14159))

(define c1 (make-complex^ n1 n2))
(define c2 (make-complex^ q1 r1))

(@>> c1 c2 (+^ c1 c2) (-^ c1 c2) (*^ c1 c2) (/^ c1 c2))
(@>> "")

(define q2 (make-rational^ (make-integer^ 40) (make-integer^ 3)))
(?true (=?^ (+^ n1 q1) q2))
(?true (=?^ n1 (make-rational^ (make-integer^ 10) (make-integer^ 1))))
)


;;; ex 2.85
(ex 85
(define (drop^ x)
  (if (pair? x)
    (let ([type-tag (car x)])
      (cond
        [(eq? type-tag (car types-list)) x]
        [(member type-tag types-list)
         (let* ([xp (project^ x)] [xr (raise^ xp)])
           (if (=?^ xr x) (drop^ xp) x))]
        [else x]))
    x))

(define (apply-generic^/v3 op . args)
  (define drop-ops '(+^ -^ *^ /^))
  (let ([rst (apply apply-generic^/v2 (cons op args))])
    (if (member op drop-ops) (drop^ rst) rst)))

(@update! ([apply-generic^ apply-generic^/v3]))
------
(install-number-packages^)
(install-tools1-to-packages^)

(define n0 (make-integer^ 0))
(define n1 (make-integer^ 3))
(define n2 (make-integer^ 10))
(define n3 (make-integer^ -10))
(define q1 (make-rational^ n2 n1))
(define q2 (make-rational^ n3 n1))
(define r1 (make-real^ 3.14159))
(define c1 (make-complex^ q1 q2))
(define c2 (make-complex^ q2 q1))
(define c3 (make-complex^ q1 q1))

(?true (=?^ (make-rational^ (make-integer^ 314159) (make-integer^ 100000))
            (drop^ r1)))
(?true (=?^ n0 (+^ q1 q2)))
(?true (=?^ n0 (+^ c1 c2)))
(?true (=?^ (make-rational^ (make-integer^ 200) (make-integer^ 9))
            (*^ c1 c3)))
)


;;; ex 2.86
(ex 86
; 2.83 已支持, 只实现了实部和虚部, 未实现模和幅角
(define (install-tools2-to-packages^)
  (define (handle proc z)
    (make-real^ (proc z)))
  (put 'sin '(integer)
    (lambda (z) (handle sin z)))
  (put 'cos '(integer)
    (lambda (z) (handle cos z)))
  (put 'sin '(rational)
    (lambda (z) (handle sin (value^ (make-rational^ (car z) (cdr z))))))
  (put 'cos '(rational)
    (lambda (z) (handle cos (value^ (make-rational^ (car z) (cdr z))))))
  (put 'sin '(real)
    (lambda (z) (handle sin z)))
  (put 'cos '(real)
    (lambda (z) (handle cos z)))
  ;()
  'done)

(define (sin^ x) (apply-generic^ 'sin x))
(define (cos^ x) (apply-generic^ 'cos x))
------
(install-number-packages^)
(install-tools1-to-packages^)
(install-tools2-to-packages^)

(define n1 (make-integer^ 5))
(define n2 (make-integer^ 3))
(define q1 (make-rational^ n1 n2))
(define c1 (make-real^ 1.57))

(?~= (sin 5) (value^ (sin^ n1)))
(?~= (cos 5) (value^ (cos^ n1)))
(?~= (sin 5/3) (value^ (sin^ q1)))
(?~= (cos 5/3) (value^ (cos^ q1)))
(?~= (sin 1.57) (value^ (sin^ c1)))
(?~= (cos 1.57) (value^ (cos^ c1)))
)
(run-ex 83 ~ 86)

; (run-ex 1 ~ 43)
; (run-ex 53 ~ )
