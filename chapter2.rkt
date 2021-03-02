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

(define (clear)
  (set-cdr! table '()))

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
              (error 'get "failed to get by type, ~a" type)))
        (error 'get "failed to get by tag, ~a" tag))))

(define deriv-proc #f)

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
)
(run-ex 74)


; (run-ex 1 ~ 42)
; (run-ex 53 ~ )