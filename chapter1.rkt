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


;;; 1.3
((lambda ()
(define (f a b c)
  (if (> a b)
      (if (> b c)
          (+ a b)
          (+ a c))
      (if (> a c)
          (+ a b)
          (+ b c))))

(hlog "ex-1.3")
(define a 10)
(define b 100)
(define c 1000)
(assert 1100 (f a b c))
(assert 1100 (f a c b))
(assert 1100 (f b a c))
(assert 1100 (f b c a))
(assert 1100 (f c a b))
(assert 1100 (f c b a))
(tlog)
))


;;; 1.4
((lambda ()
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(hlog "ex-1.4")
(assert 9 (a-plus-abs-b 4 5))
(assert 9 (a-plus-abs-b 4 -5))
(tlog)
))


;;; 1.5
((lambda ()
(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

(hlog "ex-1.5")
;; infi loop in applicative-order
;; return 0 in normal-order
;; (assert 0 (test 0 (p)))
(tlog)
))


;;; 1.6
((lambda ()
(hlog "ex-1.6")
;; infi loop
(define (iff predicate? then-clause else-clause)
  (cond [predicate? then-clause]
        [else else-clause]))
(if #t (mprintln "True") (mprintln "False"))
(displayln "******")
(iff #t (mprintln "True") (mprintln "False"))
(tlog)
))


;;; 1.7
(define (sqrt-common-iter guess x good-enough? improve)
  (if (good-enough? guess x)
      guess
      (sqrt-common-iter (improve guess x) x good-enough? improve)))

((lambda ()
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) 0.001))

(define (test x good-enough?)
  (mprintln x (sqrt x) (sqrt-common-iter 1.0 x good-enough? improve) "~~~~~~"))

(hlog "ex-1.7")
(test 2 good-enough?)
(test 2e-20 good-enough?)
;; (test 2e20 good-enough?)   ; very slow

((lambda ()
(define (good-enough? guess x)
  (let ([new-guess (improve guess x)])
    (if (= new-guess guess)
        #t
        (< (/ (abs (- new-guess guess)) (+ new-guess guess)) 1e-10))))

(test 2 good-enough?)
(test 2e-20 good-enough?)
(test 2e20 good-enough?)
))
(tlog)
))


;;; 1.8
((lambda ()
(define (cubic-root-iter guess x)
  (let ([new-guess (improve guess x)])
    (if (good-enough? new-guess guess)
        new-guess
        (cubic-root-iter new-guess x))))

(define (good-enough? new-guess guess)
  (if (= new-guess guess)
      #t
      (< (/ (abs (- new-guess guess)) (+ new-guess guess)) 1e-10)))

(define (improve guess x)
  (/ (+ (/ x (sqr guess)) (* 2 guess)) 3))

(define (test x)
  (define a (cubic-root-iter 1.0 x))
  (mprintln a (* a a a) x "~~~~~~"))

(hlog "ex-1.8")
(test 10)
(test 2e-20)
(test 2e20)
(tlog)
))


;;; 1.9
((lambda ()
((lambda ()
(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))
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
(void)
))

((lambda ()
(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
(void)
))
(mlog "ex-1.9")
))


;;; 1.10
((lambda ()
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(hlog "ex-1.10")
(assert 1024 (A 1 10))
(assert 65536 (A 2 4))
(assert 65536 (A 3 3))

(define (f n) (A 0 n))
; 2 * n
(define (g n) (A 1 n))
; 2 ^ n
(define (h n) (A 2 n))
; 2 ^ (2 ^ ... (2 ^ 2))

(assert (* 2 20) (f 20))
(assert (expt 2 20) (g 20))
(assert 16 (h 3))

(tlog)
))


;;; 1.11
((lambda ()
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-iter-inner a0 a1 a2 count)
    (if (> count n)
        a2
        (f-iter-inner a1 a2 (+ (* 3 a0) (* 2 a1) a2) (inc count))))
  (if (< n 3)
      n
      (f-iter-inner 0 1 2 3)))

(hlog "ex-1.11")
(assert 59 (f-rec 6))
(assert 59 (f-iter 6))
(tlog)
))


;;; 1.12
((lambda ()
(define (pascal n)
  (define (pascal-element row col)
    (cond
      [(or (= row 0) (= col 0)) 1]
      [else (+ (pascal-element (dec row) col)
               (pascal-element row (dec col)))]))

  (define (print-row row from)
    (if (> (+ row from) (- n 1))
        (void)
        (begin
          (printf "~a\t" (pascal-element row from))
          (print-row row (inc from)))))

  (define (print-pascal from)
    (if (> from (- n 1))
      (void)
      (begin
        (print-row from 0)
        (printf "\n")
        (print-pascal (inc from)))))

  (print-pascal 0)
)

(hlog "ex-1.12")
(pascal 10)
(tlog)
))


;;; 1.13
((lambda ()
(define a (sqrt 5.0))

(define phi (/ (+ 1 a) 2))

(define (f n)
  (/ (expt phi n) a))

(define (fib n)
  (define (fib-iter a0 a1 idx)
    (if (> idx n)
        a1
        (fib-iter a1 (+ a0 a1) (inc idx))))

  (if (< n 2)
      n
      (fib-iter 0 1 2)))

(define (test n)
  (mprintln (f n) (fib n)))

(hlog "ex-1.13")
(test 10)
(test 20)
(tlog)
))


;;; 1.14
; 11 (50 25 10 5 1)
; -39 (50 25 10 5 1) + 11 (25 10 5 1)
; 0 + -14 (25 10 5 1) + 11 (10 5 1)
; 0 + 0 + 1 (10 5 1) + 11 (5 1)
; 0 + 0 + -9 (10 5 1) + 1 (5 1) + 6 (5 1) + 11 (1)
; 0 + 0 + 0 + -4 (5 1) + 1 (1) + 1 (5 1) + 6 (1) + 1
; 0 + 0 + 0 + 0 + 1 + -4 (5 1) + 1 (1) + 5 (1) + 1
; 0 + 0 + 0 + 0 + 1 + 0 + 1 + 1 + 1
; 4


;;; 1.15
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))


;;; 1.16
((lambda ()
(define (fast-expt b n)
  (define (fast-expt-iter bb nn rst)
    (cond [(= nn 0) rst]
          [(even? nn) (fast-expt-iter (* bb bb) (/ nn 2) rst)]
          [else (fast-expt-iter bb (dec nn) (* rst bb))]))

  (fast-expt-iter b n 1)
)

(define (test b n)
  (mprintln (expt b n) (fast-expt b n)))

(hlog "ex-1.16")
(test 4 5)
(test 3 20)
(tlog)
))


;;; 1.17
((lambda ()
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-prod-rec a b)
  (cond [(= b 1) a]
        [(even? b) (fast-prod-rec (double a) (halve b))]
        [else (+ a (fast-prod-rec a (dec b)))]))

(define (test a b)
  (mprintln (* a b) (fast-prod-rec a b)))

(hlog "ex-1.17")
(test 10 20)
(test 12345 6789)
(tlog)
))


;;; 1.18
((lambda ()
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-prod a b)
  (define (fast-prod-iter aa bb rst)
    (cond [(= bb 0) rst]
          [(even? bb) (fast-prod-iter (double aa) (halve bb) rst)]
          [else (fast-prod-iter aa (dec bb) (+ rst aa))]))

  (fast-prod-iter a b 0)
)

(define (test a b)
  (mprintln (* a b) (fast-prod a b)))

(hlog "ex-1.18")
(test 10 20)
(test 12345 6789)
(tlog)
))


;;; 1.19
((lambda ()
(define (fib-normal n)
  (define (fib-iter a0 a1 idx)
    (if (> idx n)
        a1
        (fib-iter a1 (+ a0 a1) (inc idx))))

  (if (< n 2)
      n
      (fib-iter 0 1 2)))

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

(define (test n)
  (mprintln (fib-normal n) (fib n)))

(hlog "ex-1.19")
(test 10)
(test 20)
(tlog)
))


;;; 1.20
; normal-order
; (gcd 206 40)
; (gcd 40 (r 206 40))
; (if (= (r 206 40) 0) (...) (...))
; +1
; (gcd (r 206 40) (r 40 (r 206 40)))
; (if (= (r 40 (r 206 40)) 0) (...) (...))
; +2
; (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; (if (= (r (r 206 40) (r 40 (r 206 40))) 0) (...) (...))
; +4
; (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; (if (= 0 (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))) (...) (...))
; +7
; (r (r 206 40) (r 40 (r 206 40)))
; +4
; count = 18
;
; applicative-order
; (gcd 206 40)
; (gcd 40 (r 206 40))
; (gcd 40 6)
; (gcd 6 (r 40 6))
; (gcd 6 4)
; (gcd 4 (r 6 4))
; (gcd 4 2)
; (gcd 2 (r 4 2))
; (gcd 2 0)
; count = 4


;;; 1.21
(define (divide? a b) (= 0 (remainder b a)))

(define (smallest-divisor n)
  (define (iter d)
    (cond [(> (sqr d) n) n]
          [(divide? d n) d]
          [else (iter (inc d))]))

  (iter 2)
)

(define (prime? n)
  (= n (smallest-divisor n)))

((lambda ()
(hlog "ex-1.21")
(assert 199 (smallest-divisor 199))
(assert 1999 (smallest-divisor 1999))
(assert 7 (smallest-divisor 19999))
(tlog)
))


;;; 1.22
(define (timed-prime-test prime? n)
  (start-prime-test prime? n (runtime)))

(define (start-prime-test prime? n start-time)
  (if (prime? n)
      (begin
        (printf "~a *** ~a\n" n (- (runtime) start-time))
        #t)
      #f))

(define (search-for-primes prime? from count)
  (define (iter x remain-count)
    (if (= remain-count 0)
        (void)
        (let ([rst (timed-prime-test prime? x)])
          (if rst
              (iter (inc x) (dec remain-count))
              (iter (inc x) remain-count)))))

  (iter (inc from) count))

((lambda ()
(hlog "ex-1.22")
(search-for-primes prime? 1000 3)
(search-for-primes prime? 10000 3)
(search-for-primes prime? 100000 3)
(search-for-primes prime? 1000000 3)
(search-for-primes prime? 10000000 3)
(tlog)
))


;;; 1.23
((lambda ()
(define (smallest-divisor n)
  (define (next d)
    (if (= d 2)
        3
        (+ d 2)))

  (define (iter d)
    (cond [(> (sqr d) n) n]
          [(divide? d n) d]
          [else (iter (next d))]))

  (iter 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(hlog "ex-1.23")
(search-for-primes prime? 1000 3)
(search-for-primes prime? 10000 3)
(search-for-primes prime? 100000 3)
(search-for-primes prime? 1000000 3)
(search-for-primes prime? 10000000 3)
(tlog)
))


;;; 1.24
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp) (remainder (sqr (expmod base (/ exp 2) m)) m)]
        [else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (fermat-test n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) #t]
        [(fermat-test n) (fast-prime? n (dec times))]
        [else #f]))

((lambda ()
(define (prime? n)
  (fast-prime? n 10))

(hlog "ex-1.24")
(search-for-primes prime? 1000 3)
(search-for-primes prime? 10000 3)
(search-for-primes prime? 100000 3)
(search-for-primes prime? 1000000 3)
(search-for-primes prime? 10000000 3)
(tlog)
))


;;; 1.25
; the number may be too big


;;; 1.26
; calculate expmod twice


;;; 1.27
((lambda ()
(define (carmichael-test n)
  (define (iter x)
    (cond [(= x n) #t]
          [(= x (expmod x n n)) (iter (inc x))]
          [else #f]))
  (iter 2))

(hlog "ex-1.27")
(assert #f (carmichael-test 560))
(assert #t (carmichael-test 561))
(assert #t (carmichael-test 1105))
(assert #t (carmichael-test 1729))
(assert #t (carmichael-test 2465))
(assert #t (carmichael-test 2821))
(assert #t (carmichael-test 6601))
(tlog)
))


;;; 1.28
((lambda ()
(define (mr-expmod base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
       (let* ([val (mr-expmod base (/ exp 2) m)]
              [rem (remainder (sqr val) m)])
         (if (and (not (= val 1))
                  (not (= val (- m 1)))
                  (= rem 1))
           0
           rem))]
    [else (remainder (* base (mr-expmod base (dec exp) m)) m)]))

(define (miller-rabin-test n)
  (define (try-it a)
    (= 1 (mr-expmod a (- n 1) n)))
  (try-it (random 2 n)))

(define (fast-prime? n times)
  (cond
    [(= times 0) #t]
    [(miller-rabin-test n) (fast-prime? n (dec times))]
    [else #f]))

(define (prime? n)
  (fast-prime? n 20))

(hlog "ex-1.28")
(assert #t (prime? 19))
(assert #t (prime? 199))
(assert #t (prime? 1999))
(assert #f (prime? 19999))
(assert #f (prime? 561))
(assert #f (prime? 1729))
(assert #f (prime? 2821))
(assert #f (prime? 6601))
(tlog)
))


;;; 1.29
(define (cube x) (* x x x))

((lambda ()
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(define(simpson-integral f a b n)
  (define h (/ (- b a) n))

  (define (coeff k)
    (cond
      [(or (= k 0) (= k n)) 1]
      [(even? k) 2]
      [else 4]))

  (define (term k)
    (* (coeff k) (f (+ a (* k h)))))

  (* (/ h 3) (sum term 0 inc n))
)

(hlog "ex-1.29")
(mprintln (integral cube 0 1 0.01))
(mprintln (integral cube 0 1 0.001))
(mprintln "~~~~~~")
(mprintln (simpson-integral cube 0 1.0 100))
(mprintln (simpson-integral cube 0 1.0 1000))
(tlog)
))


;;; 1.30
((lambda ()
(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ (term x) result))))
  (iter a 0))

(hlog "ex-1.30")
(assert 55 (sum identity 1 inc 10))
(tlog)
))


;;; 1.31
((lambda ()
(define (factorial n product)
  (product identity 1 inc n))

(define (estimate-pi n product)
  (define (numerator n)
    (if (even? n)
        (+ n 2)
        (+ n 1)))

  (define (denominator n)
    (if (even? n)
        (+ n 1)
        (+ n 2)))

  (exact->inexact
    (* 4 (/ (product numerator 1 inc n)
            (product denominator 1 inc n))))
)

(hlog "ex-1.31")

((lambda ()
(define (product term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* (term x) result))))
  (iter a 1))

(assert 720 (factorial 6 product))
(mprintln (estimate-pi 100 product)
          (estimate-pi 1000 product))
))


((lambda ()
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b)))
)

(assert 720 (factorial 6 product))
(mprintln (estimate-pi 100 product)
          (estimate-pi 1000 product))
))

(tlog)
))


;;; 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner result (term x)))))
  (iter a null-value)
)

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate-rec combiner null-value term (next a) next b) (term a)))
)

((lambda ()
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))

(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))

(hlog "ex-1.32")
(assert 55 (sum identity 1 inc 10))
(assert 55 (sum-rec identity 1 inc 10))
(assert 720 (product identity 1 inc 6))
(assert 720 (product-rec identity 1 inc 6))
(tlog)
))


;;; 1.33
(define (filtered-accumulate combiner null-value term a next b pred?)
  (define (iter x result)
    (cond
      [(> x b) result]
      [(pred? x) (iter (next x) (combiner result (term x)))]
      [else (iter (next x) result)]))
  (iter a null-value))

((lambda ()
(define (sum-of-primes a b)
  (filtered-accumulate + 0 identity a inc b prime?))

(define (product-of-relative-primes n)
  (define (relative-prime? x)
    (= 1 (gcd x n)))
  (filtered-accumulate * 1 identity 1 inc (- n 1) relative-prime?))

(hlog "ex-1.33")
(assert 1060 (sum-of-primes 2 100))
(assert (* 3 7 9 11 13 17 19) (product-of-relative-primes 20))
(tlog)
))


;;; 1.34
((lambda ()
(define (f g) (g 2))

(hlog "ex-1.34")
(mprintln
  (f sqr)
  (f (lambda (z) (* z (+ z 1)))))
; (f f) => (f 2) => (2 2) => error
(tlog)
))


;;; 1.35
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

((lambda ()
(mlog "ex-1.35"
 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
))


;;; 1.36
((lambda ()
(define (fixed-point-with-print f first-guess next-gen)
  (define tolerance 1e-5)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try idx guess)
    (printf "~a\t~a\n" idx guess)
    (let ([next (next-gen guess)])
      (if (close-enough? guess next)
          next
          (try (inc idx) next))))
  (try 1 first-guess)
)

(define (log-root x)
  (/ (log 1000) (log x)))

(hlog "ex-1.36")
(fixed-point-with-print log-root 2.0 log-root)
(mprintln "~~~~~~")
(fixed-point-with-print log-root 2.0 (lambda (x) (average x (log-root x))))
(tlog)
))


;;; 1.37
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (cont-frac-rec n d k)
  (define (recur i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (recur (inc i))))))
  (recur 1))

(define cont-frac cont-frac-iter)

((lambda ()
(define (estimate-phi count)
  (cont-frac-iter (lambda (i) 1.0)
             (lambda (i) 1.0)
             count))

(hlog "ex-1.37")
(mprintln (estimate-phi 10)
          (estimate-phi 100)
          (estimate-phi 1000)
          "~~~~~~")

((lambda ()
(define (estimate-phi count)
  (cont-frac-rec (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 count))

(mprintln (estimate-phi 10)
          (estimate-phi 100)
          (estimate-phi 1000))
))
(tlog)
))


;;; 1.38
((lambda ()
(define (n i) 1.0)

(define (d i)
  (let ([rem (remainder i 3)])
    (if (= rem 2)
      (+ 2 (* 2 (quotient i 3)))
      1)))

(define (estimate-e k)
  (+ 2 (cont-frac n d k)))

(mlog "ex-1.38"
  (estimate-e 10)
  (estimate-e 100)
  (estimate-e 1000)
)
))


;;; 1.39
((lambda ()
(define (tan-cf x k)
  (exact->inexact
    (cont-frac
      (lambda (i)
        (if (= i 1)
          x
          (- (* x x))))
      (lambda (i) (- (* 2 i) 1))
      k)))

(mlog "ex-1.39"
  (tan (/ pi 4))
  (tan-cf (/ pi 4) 1000)
  "~~~"
  (tan 2)
  (tan-cf 2 1000)
)
))


;;; 1.40
(define (deriv g)
  (define dx 1e-5)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

((lambda ()
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(define (test a b c)
  (define x (newtons-method (cubic a b c) 1.0))
  (mprintln
    x
    ((cubic a b c) x)
    "~~~~~~"))

(hlog "ex-1.40")
(test 1 1 1)
(test 1 10 100)
(tlog)
))


;;; 1.41
((lambda ()
(define (double f)
  (lambda (x)
    (f (f x))))

(hlog "ex-1.41")
(assert 12 ((double inc) 10))
(assert 21 (((double (double double)) inc) 5))
(tlog)
))


;;; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

((lambda ()
(hlog "ex-1.42")
(assert 49 ((compose sqr inc) 6))
(tlog)
))


;;; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

((lambda ()
(hlog "ex-1.43")
(assert 625 ((repeated sqr 2) 5))
(tlog)
))


;;; 1.44
(define (smooth f)
  (define dx 1e-6)
  (lambda (x)
    (average
      (f (- x dx))
      (f x)
      (f (+ x dx)))))

(define (smooth-nth n)
  (repeated smooth n))

((lambda ()
(hlog "ex-1.44")
(mprintln
  ((smooth sqr) 2.0)
  (((smooth-nth 3) sqr) 2.0))
(tlog)
))


;;; 1.45
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

((lambda ()
(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))

(define (4th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y)))) 1.0))

(define (nth-root-spec-rcnt x n rcnt)
  (fixed-point
    ((repeated average-damp rcnt)
      (lambda (y) (/ x (expt y (- n 1)))))
    1.0))

(define (nth-root x n)
  (define (repeated-count n result)
    (if (= n 1)
      result
      (repeated-count (quotient n 2) (inc result))))
  (let ([rcnt (repeated-count n 0)])
    (nth-root-spec-rcnt x n rcnt)))

(define (test f x n)
  (let ([y (f x)])
    (printf "~a ^ ~a == ~a\n" y n x)
    (assert-close-enough x (expt y n))))

(define (test-nth-rcnt x n rcnt)
  (test
    (lambda (x) (nth-root-spec-rcnt x n rcnt))
    x
    n))

(define (test-nth x n)
  (test
    (lambda (x) (nth-root x n))
    x
    n))

(hlog "ex-1.45")
(test square-root 12300 2)
(test cube-root 12300 3)
(test 4th-root 12300 4)
(printf "~~~~~~\n")
(test-nth-rcnt 12300 2 1)
(test-nth-rcnt 12300 3 1)
(test-nth-rcnt 12300 4 2)
(test-nth-rcnt 12300 16 4)
(printf "~~~~~~\n")
(test-nth 12300 2)
(test-nth 12300 3)
(test-nth 12300 4)
(test-nth 12300 5)
(test-nth 12300 8)
(test-nth 12300 10)
(test-nth 12300 15)
(test-nth 12300 16)
(test-nth 12300 17)
(tlog)
))


;;; 1.46
(define (iteractive-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iteractive-improve good-enough? improve) (improve guess)))))

((lambda ()
(define (close-enough? a b)
  (define tolerance 1e-3)
  (cond [(= a b) #t]
        [(< (/ (abs (- a b)) (/ (+ a b) 2)) tolerance) #t]
        [else #f]))

(define (sqrt-ii x)
  (define (improve y)
    (average y (/ x y)))
  ((iteractive-improve
     (lambda (guess) (close-enough? guess (improve guess)))
     improve)
   1.0))

(define (fixed-point-ii f first-guess)
  ((iteractive-improve
     (lambda (guess) (close-enough? guess (f guess)))
     f)
    first-guess))

(hlog "ex-1.46")
(mprintln
  (sqrt-ii 2)
  (fixed-point-ii cos 1.0))
(tlog)
))