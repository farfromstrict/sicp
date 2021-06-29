#lang sicp
(#%require "lib/libs.rkt")
(#%require racket)


;;; ex 1.1
(ex 1
(?== 10 10)
(?== 12 (+ 5 3 4))
(?== 8 (- 9 1))
(?== 3 (/ 6 2))
(?== 6 (+ (* 2 4) (- 4 6)))

(define a 3)
(define b (+ a 1))

(?== 19 (+ a b (* a b)))
(?false (= a b))
(?== 4 (if (and (> b a) (< b (* a b)))
         b
         a))
(?== 16 (cond ((= a 4) 6)
              ((= b 4) (+ 6 7 a))
              (else 25)))
(?== 6 (+ 2 (if (> b a) b a)))
(?== 16 (* (cond ((> a b) a)
                 ((< a b) b)
                 (else -1))
           (+ a 1)))
)


;;; ex 1.2
(ex 2
(?== -37/150
     (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
        (* 3 (- 6 2) (- 2 7))))
)


;;; ex 1.3
(ex 3
(define (sum-of-bigger-num a b c)
  (cond
    [(>= a b)
     (cond
       [(>= b c) (+ a b)]
       [else (+ a c)])]
    [else
     (cond
       [(>= a c) (+ a b)]
       [else (+ b c)])]))

(?== 5 (sum-of-bigger-num 1 2 3))
(?== 5 (sum-of-bigger-num 1 3 2))
(?== 5 (sum-of-bigger-num 2 1 3))
(?== 5 (sum-of-bigger-num 2 3 1))
(?== 5 (sum-of-bigger-num 3 1 2))
(?== 5 (sum-of-bigger-num 3 2 1))
)


;;; ex 1.4
(ex 4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(?== 15 (a-plus-abs-b 10 5))
(?== 15 (a-plus-abs-b 10 -5))
)


;;; ex 1.5
(ex 5
(define (p) (p))  ; 无限循环

(define (test x y)
  (if (= x 0) 0 y))

;(test 0 (p))
; 正则序: (p) 不会被求值
; 应用序: (p) 会被求值, 程序不会结束
)


;;; ex 1.6
(ex 6
(define (new-if predicate then-clause else-clause)
  (cond
    [predicate then-clause]
    [else else-clause]))
; new-if 是普通 procedure, 在应用序下, predicate & then-clause & else-clause 都会被先求值
; 这会造成资源浪费, 甚至进入死循环

(define lst '())
(define (foo1) (set! lst (cons 1 lst)) 1)
(define (foo2) (set! lst (cons 2 lst)) 2)

(?== 1 (new-if #t (foo1) (foo2)))
(?== '(2 1) lst)
)



;;; ex 1.7
(ex 7
(define tolerance 1e-6)

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) tolerance))

(define (close-enough? v1 v2)
  (< (diff-ratio v1 v2) tolerance))
------
(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(define (verify x tolerance [rst #t])
  (let* ([root (sqrt x)]
         [sqr-root (sqr root)]
         [ratio (diff-ratio sqr-root x)])
    (printf "sqrt(~a) = ~a\n" x root)
    (printf "sqr(~a) = ~a (~a)\n" root sqr-root ratio)
    (?== rst (< ratio tolerance))))

(verify 1e-6 tolerance #f)
;(verify 1e100)  ; 效率极低

(define (sqrt-iter/v2 guess x)
  (let ([new-guess (improve guess x)])
    (if (close-enough? guess new-guess)
        new-guess
        (sqrt-iter/v2 new-guess x))))

(@update ([sqrt-iter sqrt-iter/v2])
  (verify 1e-6 tolerance)
  (verify 1e100 tolerance))
)


;;; ex 1.8
(ex 8
(define (cubic-root x)
  (define (improve y)
    (/ (+ (/ x (* y y))
          (* 2 y))
       3))
  (let loop ([guess 1.0])
    (let ([next (improve guess)])
      (if (close-enough? next guess)
          next
          (loop next)))))

(define (verify x)
  (let* ([root (cubic-root x)]
         [cubic (* root root root)])
    (printf "cubrt(~a) = ~a\n" x root)
    (?~=% x cubic tolerance)))

(verify 1e-6)
(verify 1e100)
)


;;; ex 1.9
(ex 9
(let ()
  (define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))))

  (?== 9 (+ 4 5))
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
)

(let ()
  (define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b))))

  (?== 9 (+ 4 5))
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
)
)


;;; ex 1.10
(ex 10
(define (A x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (A (- x 1)
                 (A x (- y 1)))]))

(?== 1024 (A 1 10))
(?== 65536 (A 2 4))
(?== 65536 (A 3 3))

; 2 * n
(define (f n) (A 0 n))
(?== (f 20) 40)

; 2 ^ n
(define (g n) (A 1 n))
(?== (g 20) (expt 2 20))

; 2 ^ (2 ^ ... (2 ^ 2))
(define (h n) (A 2 n))
(?== (h 3) 16)
)


;;; ex 1.11
(ex 11
(define (fn-rec n)
  (cond
    [(< n 3) n]
    [else (+ (fn-rec (- n 1))
             (fn-rec (- n 2))
             (fn-rec (- n 3)))]))

(define (fn-iter n)
  (cond
    [(< n 3) n]
    [else
     (let loop ([i 2] [a0 0] [a1 1] [a2 2])
       (if (= i n)
           a2
           (loop (inc i) a1 a2 (+ a0 a1 a2))))]))

(define rst-list '(0 1 2 3 6 11 20 37 68 125))

(?== (map fn-rec (int/list 0 9)) rst-list)
(?== (map fn-iter (int/list 0 9)) rst-list)
)


;;; ex 1.12
(ex 12
(define (pascal-value row col)
  (cond
    [(or (= row 1) (= col 1)) 1]
    [else (+ (pascal-value (dec row) col)
             (pascal-value row (dec col)))]))

(define (pascal-triangle n)
  (for ([row (int/list 1 n)])
    (for ([col (int/list 1 (- n (- row 1)))])
      (let ([pv (pascal-value row col)])
        (cond
          [(< pv 10) (printf "~a    " pv)]
          [(< pv 100) (printf "~a   " pv)]
          [else (printf "~a  " pv)])))
    (printf "\n")))

(pascal-triangle 8)
)


;;; ex 1.13
(ex 13
(define phi (/ (+ 1 (sqrt 5)) 2))
(define eta (/ (- 1 (sqrt 5)) 2))
------
(define (fib-iter n)
  (let loop ([m 1] [a1 1] [a2 1])
    (cond
      [(= n m) a1]
      [else (loop (inc m) a2 (+ a1 a2))])))

(define (fib-ins n)
  (exact-round
    (/ (- (expt phi n)
          (expt eta n))
       (sqrt 5))))

(for/list ([i (int/list 1 10)])
  (?== (fib-iter i)
       (fib-ins i)))

; fib(0) = fib-ins(0) = 1
; fib(1) = fib-ins(1) = 1
; fib(n+1) = fib(n) + fib(n-1) = fib-ins(n) + fib-ins(n-1)
;          = ((phi ^ n) - (eta ^ n)) / (sqrt 5) + ((phi ^ (n-1)) - (eta ^ (n-1))) / (sqrt 5)
;          = (((phi ^ n) + (phi ^ (n-1))) - ((eta ^ n) + (eta ^ (n-1)))) / (sqrt 5)
;          = (((phi ^ (n-1)) * (phi + 1)) + ((eta ^ (n-1)) * (eta + 1))) / (sqrt 5)
; (phi ^ 2) = (3 + (sqrt 5)) / 2 = phi + 1
; (eta ^ 2) = (3 - (sqrt 5)) / 2 = eta + 1
; fib(n+1) = (((phi ^ (n-1)) * (phi ^ 2)) + ((eta ^ (n-1)) * (eta ^ 2))) / (sqrt 5)
;          = ((phi ^ (n+1)) + (eta ^ (n+1))) / (sqrt 5)
;          = fib-ins(n+1)
; fib(n) = (phi ^ n)/(sqrt 5) - (eta ^ n)/(sqrt 5)
; delta = |fib(n) - (phi ^ n)/(sqrt 5)| = |(eta ^ n)|/(sqrt 5) = (|eta| ^ n)/(sqrt 5)
; eta = -0.618
; delta = (|eta| ^ n)/(sqrt 5) <= 1/(sqrt 5) < 0.5
; |fib(n) - (phi ^ n)/(sqrt 5)| < 0.5
)


;;; ex 1.14
(ex 14)


;;; ex 1.15
(ex 15
(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (define tolerance 0.1)
  (define (small-enough? a) (< (abs a) tolerance))

  (if (small-enough? angle)
      angle
      (p (sine (/ angle 3.0)))))

(?~=% (sine 12.15) (sin 12.15) 0.02)
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))
; (p (p (p (p (p 0.05)))))
; p 被使用 5 次
;
; O(a) = log(a)
)


;;; ex 1.16
(ex 16
(define (fast-expt b n)
  (let loop ([b b] [n n] [rst 1])
    (cond
      [(= n 0) rst]
      [(odd? n) (loop b (dec n) (* rst b))]
      [else (loop (* b b) (/ n 2) rst)])))

(?== (fast-expt 2 10) 1024)
(?== (fast-expt 3 10) 59049)
)


;;; ex 1.17
(ex 17
(define (double x) (arithmetic-shift x 1))
(define (halve x) (arithmetic-shift x -1))
------
(define (fast-multiple-rec a b)
  (cond
    [(= b 0) 0]
    [(odd? b) (+ a (fast-multiple-rec a (dec b)))]
    [else (fast-multiple-rec (double a) (halve b))]))

(define (verify)
  (let* ([a (random 1000)]
         [b (random 1000)]
         [rst (fast-multiple-rec a b)])
    (printf "~a * ~a = ~a\n" a b rst)
    (?== rst (* a b))))

(for/list ([i (int/list 1 3)])
  (verify))
)


;;; ex 1.18
(ex 18
(define (fast-multiple a b)
  (let loop ([a a] [b b] [rst 0])
    (cond
      [(= b 0) rst]
      [(odd? b) (loop a (dec b) (+ rst a))]
      [else (loop (double a) (halve b) rst)])))

(define (verify)
  (let* ([a (random 1000)]
         [b (random 1000)]
         [rst (fast-multiple a b)])
    (printf "~a * ~a = ~a\n" a b rst)
    (?== rst (* a b))))

(for/list ([i (int/list 1 3)])
  (verify))
)


;;; ex 1.19
(ex 19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))  ;compute p'
                   (+ (* 2 p q) (* q q))  ;compute q'
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))

(define rst-list '(1 1 2 3 5 8 13 21 34 55))

(?== (map fib (int/list 1 10)) rst-list)
)


;;; ex 1.20
(ex 20
(define count 0)

(define (remainder/v2 a b)
  (set! count (+ count 1))
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder/v2 a b))))

; 正则序
; [count= 0] (gcd 206 40)
; [count= 0] (gcd 40 (r 206 40))
; [count= 1] (gcd (r 206 40) (r 40 (r 206 40))) ;; if 判断
; [count= 3] (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; [count= 7] (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; [count=14] (r (r 206 40) (r 40 (r 206 40)))
; [count=18] 2

; 应用序
; [count=0] (gcd 206 40)
; [count=1] (gcd 40 6)
; [count=2] (gcd 6 4)
; [count=3] (gcd 4 2)
; [count=4] (gcd 2 0)
; [count=4] 2

(?== 2 (gcd 206 40))
(?== 4 count)
)


;;; ex 1.21
(ex 21
(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor d)
    (cond
      [(> (sqr d) n) n]
      [(divides? d n) d]
      [else (find-divisor (inc d))]))
  (find-divisor 2))

(define (prime? n) (= (smallest-divisor n) n))
------
(?== (smallest-divisor 199) 199)
(?== (smallest-divisor 1999) 1999)
(?== (smallest-divisor 19999) 7)
)


;;; ex 1.22
(ex 22
(define (timed-prime? n)
  (define (start-prime-test n start-time)
    (cond
      [(prime? n)
       (printf "~a *** ~a\n" n (- (runtime) start-time))
       #t]
      [else #f]))
  (start-prime-test n (runtime)))

(define (search-for-primes start count)
  (cond
    [(> count 0)
     (cond
       [(timed-prime? start) (search-for-primes (inc start) (dec count))]
       [else (search-for-primes (inc start) count)])]))
------
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
)


;;; ex 1.23
(ex 23
(define (smallest-divisor/v2 n)
  (define (next test-divisor)
    (if (= test-divisor 2) 3 (+ test-divisor 2)))
  (define (find-divisor test-divisor)
    (cond
      [(> (sqr test-divisor) n) n]
      [(divides? test-divisor n) test-divisor]
      [else (find-divisor (next test-divisor))]))
  (find-divisor 2))

(@update ([smallest-divisor smallest-divisor/v2])
  (search-for-primes 100000 3)
  (search-for-primes 1000000 3)
  (search-for-primes 10000000 3))
)


;;; ex 1.24
(ex 24
(define (expmod base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
     (remainder (sqr (expmod base (/ exp 2) m)) m)]
    [else
     (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    [(= times 0) #t]
    [(fermat-test n)
     (fast-prime? n (dec times))]
    [else #f]))
------
(define (prime/v2? n) (fast-prime? n 10))

(@update ([prime? prime/v2?])
  (search-for-primes 100000 3)
  (search-for-primes 1000000 3)
  (search-for-primes 10000000 3))
)


;;; ex 1.25
(ex 25
; 这一方式不合适: 直接计算乘幂可能会产生极大的数, 带来性能或存储精度问题
)


;;; ex 1.26
(ex 26
; expmod 计算量加倍
)


;;; ex 1.27
(ex 27
(define carmichael-numbers (list 561 1105 1729 2465 2821 6601))
------
(?== 3 (smallest-divisor 561))
(?== 5 (smallest-divisor 1105))
(?== 7 (smallest-divisor 1729))
(?== 5 (smallest-divisor 2465))
(?== 7 (smallest-divisor 2821))
(?== 7 (smallest-divisor 6601))

(define (full-fermat-test n)
  (define (pass? a)
    (= a (expmod a n n)))
  (let loop ([a 2])
    (cond
      [(= a n) #t]
      [(pass? a) (loop (inc a))]
      [else #f])))

(for/list ([n carmichael-numbers])
  (?true (full-fermat-test n)))
)


;;; ex 1.28
(ex 28
(define (expmod/v2 base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
     (let* ([val (expmod/v2 base (/ exp 2) m)]
            [rem (remainder (sqr val) m)])
       (if (and (not (= val 1))
                (not (= val (- m 1)))
                (= rem 1))
           0
           rem))]
    [else (remainder (* base (expmod/v2 base (dec exp) m)) m)]))

(define (miller-rabin-test n)
  (define (try-it a)
    (= 1 (expmod/v2 a (- n 1) n)))
  (try-it (random 2 n)))

(define (fast-prime? n times)
  (cond
    [(= times 0) #t]
    [(miller-rabin-test n) (fast-prime? n (dec times))]
    [else #f]))

(define (prime? n) (fast-prime? n 10))

(?true (prime? 19))
(?true (prime? 199))
(?true (prime? 1999))
(?false (prime? 19999))

(for/list ([n carmichael-numbers])
  (?false (prime? n)))
)


;;; ex 1.29
(ex 29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))
------
(define (cube x) (* x x x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (c k)
    (cond
      [(= k 0) 1]
      [(odd? k) 4]
      [else 2]))
  (* (/ h 3)
     (sum (lambda (k) (* (c k) (y k))) 0 inc n)))

(?~=% 0.25 (integral cube 0 1 0.01) 1e-4)
(?~=% 0.25 (integral cube 0 1 0.001) 1e-6)
(?~=% 0.25 (simpson-integral cube 0 1.0 100) 0.02)
(?~=% 0.25 (simpson-integral cube 0 1.0 1000) 0.002)
)


;;; ex 1.30
(ex 30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(?== 55 (sum identity 1 inc 10))
)


;;; ex 1.31
(ex 31
(define (product/rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product/rec term (next a) next b))))

(define (product term a next b)
  (let loop ([a a] [result 1])
    (if (> a b)
        result
        (loop (next a) (* result (term a))))))

(define (factorial n) (product identity 1 inc n))

(?== 120 (product/rec identity 1 inc 5))
(?== 720 (product/rec identity 1 inc 6))

(?== 120 (factorial 5))
(?== 720 (factorial 6))

(define (pi-estimate n)
  (define (term i)
    (cond
      [(odd? i) (/ (+ i 1.0) (+ i 2.0))]
      [else (/ (+ i 2.0) (+ i 1.0))]))
  (* 4.0
     (product term 1 inc n)))

(?~=% 3.14159 (pi-estimate 1000) 0.001)
)


;;; 1.32
(ex 32
(define (accumulate/rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate/rec combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (let loop ([a a] [result null-value])
    (if (> a b)
        result
        (loop (next a) (combiner (term a) result)))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(?== 55 (accumulate/rec + 0 identity 1 inc 10))
(?== 720 (accumulate/rec * 1 identity 1 inc 6))
(?== 55 (sum identity 1 inc 10))
(?== 720 (product identity 1 inc 6))
)


;;; 1.33
(ex 33
(define (filtered-accumulate pred? combiner null-value term a next b)
  (let loop ([a a] [result null-value])
    (cond
      [(> a b) result]
      [(pred? a) (loop (next a) (combiner (term a) result))]
      [else (loop (next a) result)])))

(define (sum-of-primes a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(?== (+ 11 13 17 19)
     (sum-of-primes 10 20))

(define (product-of-relative-primes n)
  (define (relative-prime? m)
    (= 1 (gcd m n)))
  (filtered-accumulate relative-prime? * 1 identity 2 inc (- n 1)))

(?== (* 3 7 9 11 13 17 19)
     (product-of-relative-primes 20))
)


;;; 1.34
(ex 34
(define (f g) (g 2))
; (f f)

; (define (f g) (g 2))
; (f f) => (f 2) => (2 2)
; 报错, 2 不是 procedure
)


;;; 1.35
(ex 35
(define (fixed-point f first-guess)
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define ((average-damp f) x)
  (average x (f x)))
------
; x = 1 + 1/x
; x ^ 2 - x - 1 = 0
; x = 1.618

(?~=% phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 1e-6)
)


;;; 1.36
(ex 36
(define (fixed-point-with-print f first-guess)
  (let loop ([guess first-guess] [count 1])
    (let ([next (f guess)])
      (printf "[~a] ~a\n" count guess)
      (if (close-enough? guess next)
          next
          (loop next (inc count))))))

(define (f x)
  (/ (log 1000) (log x)))

(define r1 (fixed-point-with-print f 2.0))
(?~=% 1000 (expt r1 r1) 1e-5)

(define r2 (fixed-point-with-print (average-damp f) 2.0))
(?~=% 1000 (expt r2 r2) 1e-5)
)


;;; 1.37
(ex 37
(define (cont-frac n d k)
  (let loop ([i k] [result 0])
    (if (= i 0)
        result
        (loop (dec i) (/ (n i) (+ (d i) result))))))
------
(define (cont-frac/rec n d k)
  (let loop ([i 1])
    (if (> i k)
        0
        (/ (n i) (+ (d i) (loop (inc i)))))))

(define (phi-frac k)
  (cont-frac (const 1.0) (const 1.0) k))

(?~=% (- phi 1) (cont-frac/rec (const 1.0) (const 1.0) 100))
(?~=% (- phi 1) (phi-frac 100))

(?~=% (- phi 1) (phi-frac 10) 1e-4)
(?~=% (- phi 1) (phi-frac 11) 5e-5)
)


;;; ex 1.38
(ex 38
(define (d i)
  (let ([rem (remainder i 3)])
    (if (= rem 2)
        (+ 2 (* 2 (quotient i 3)))
        1)))

(define (estimate-e k)
  (+ 2 (cont-frac (const 1.0) d k)))

(?~=% (exp 1) (estimate-e 10))
(?~=% (exp 1) (estimate-e 100))
)


;;; ex 1.39
(ex 39
(define (tan-cf x k)
  (cont-frac
    (lambda (i) (if (= i 1) x (- (sqr x))))
    (lambda (i) (- (* 2 i) 1))
    k))

(?~=% (tan 0.5) (tan-cf 0.5 10))
(?~=% (tan 0.5) (tan-cf 0.5 20))
)


;;; ex 1.40
(ex 40
(define (newtons-method g guess)
  (define dx 1e-6)
  (define ((deriv g) x)
    (/ (- (g (+ x dx)) (g x)) dx))
  (define ((newtons-transform g) x)
    (- x (/ (g x) ((deriv g) x))))
  (fixed-point (newtons-transform g) guess))
------
(define ((cubic a b c) x)
  (+ (* x x x) (* a x x) (* b x) c))

(define (verify a b c)
  (define r (newtons-method (cubic a b c) 1.0))
  (@>> r)
  (?~= 0 ((cubic a b c) r)))

(verify 1 1 1)
(verify 1 10 100)
)


;;; ex 1.41
(ex 41
(define ((double f) x) (f (f x)))

(?== 3 ((double inc) 1))
(?== 21 (((double (double double)) inc) 5))
; ((double (double double)) +1)
; ((double double) ((double double) +1))
; ((double double) (double (double +1)))
; ((double double) (double +2))
; ((double double) +4)
; (double (dobule +4))
; (double +8)
; +16
)


;;; ex 1.42
(ex 42
(define ((compose f g) x)
  (f (g x)))
------
(?== 49 ((compose sqr inc) 6))
)


;;; ex 1.43
(ex 43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
------
(?== 625 ((repeated sqr 2) 5))
)


;;; ex 1.44
(ex 44
(define ((smooth f) x)
  (define dx 1e-6)
  (average (f x)
           (f (+ x dx))
           (f (- x dx))))

(define (smooth-n-times n)
  (repeated smooth n))

(@>> ((smooth sqr) 2.0))
(@>> (((smooth-n-times 3) sqr) 2.0))
)


;;; ex 1.45
(ex 45
(define (nth-root n x)
  (define (f y) (/ x (expt y (- n 1))))
  (define damp-count
    (let loop ([m 2] [result 1])
      (if (>= m n)
          result
          (loop (* m 2) (+ result 1)))))
  (fixed-point ((repeated average-damp damp-count) f) 1.0))

(define (square-root x) (nth-root 2 x))
(?~=% (sqrt 2) (square-root 2))

(define (verify n)
  (define x 12300)
  (define r0 (expt x (/ 1.0 n)))
  (define r1 (nth-root n x))
  (?~=% r1 r0))

(for/list ([n (int/list 3 10)])
  (verify n))
)


;;; ex 1.46
(ex 46
(define ((iterative-improve good-enough? improve) guess)
  (let ([next (improve guess)])
    (if (good-enough? next guess)
        next
        ((iterative-improve good-enough? improve) next))))

(define (sqrt/v2 x)
  (define (f r) (/ x r))
  ((iterative-improve close-enough? (average-damp f)) 1.0))

(?~=% (sqrt 2.0) (sqrt/v2 2.0))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? (average-damp f)) first-guess))

(?~=% phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
)


(run-ex 1 ~ 46)