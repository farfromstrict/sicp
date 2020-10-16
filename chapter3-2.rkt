#lang sicp
(#%require racket)
(#%require "common.rkt")


(define (streams-map proc . argstreams)
  (let ([null-streams (filter stream-null? argstreams)])
    (if (> (length null-streams) 0)
      empty-stream
      (stream-cons
        (apply proc (map stream-car argstreams))
        (apply streams-map
          (cons proc (map stream-cdr argstreams)))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    empty-stream
    (stream-cons low (stream-enumerate-interval (+ low 1) high))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (add-streams . ss)
  (apply streams-map (cons + ss)))

(define (mul-streams . ss)
  (apply streams-map (cons * ss)))

(define (integers-from n)
  (define s (stream-cons n (stream-map inc s)))
  s)

(define integers (integers-from 1))

(define (partial-sums s)
  (define ps
    (stream-cons
      (stream-car s)
      (add-streams ps (stream-cdr s))))
  ps)


;;; 3.50
((lambda ()
(define s1 (stream-cons 1 s1))
(define s2 (stream-cons 2 s2))
(define s3 (stream-cons 3 s3))

(define sa (streams-map + s1 s2))
(define sb (streams-map * s1 s2 s3))

(mlog "ex-3.50"
  (stream->flist sa 5)  ; => (3 3 3 3 3)
  (stream->flist sb 5)  ; => (6 6 6 6 6)
)
))


;;; 3.51
((lambda ()
(define (show x)
  (printf "[SHOW] ~a\n" x)
  x)

(define (stream-ref s i)
  (if (= i 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- i 1))))

(hlog "ex-3.51")
(define s1 (stream-enumerate-interval 0 10))
(define s2 (stream-map show s1))

(stream-ref s2 5)
(stream-ref s2 7)
(tlog)
))


;;; 3.52
((lambda ()
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

(mlog "ex-3.52"
  ; sum = 0
  (stream->list seq)
  ; sum = 1 + ... + 20 = 210
  (stream-ref y 7)  ; 1 + ... + 16 = 136
  (stream->list z)
)
))


;;; 3.53
((lambda ()
(define s1 (stream-cons 1 (add-streams s1 s1)))

(mlog "ex-3.53"
  (stream->flist s1 5)  ; => (1 2 4 8 16)
)
))


;;; 3.54
((lambda ()
(define factorials 
  (stream-cons 1 (mul-streams factorials (integers-from 2))))

(mlog "ex-3.54"
  (stream->flist factorials 5)  ; => (1 2 6 24 120)
)
))


;;; 3.55
((lambda ()
(define s1 (partial-sums integers))

(mlog "ex-3.55"
  (stream->flist s1 5)  ; => (1 3 6 10 15)
)
))


;;; 3.56
(define (merge s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
      (let ([s1car (stream-car s1)]
            [s2car (stream-car s2)])
        (cond
          [(< s1car s2car)
             (stream-cons s1car (merge (stream-cdr s1) s2))]
          [(> s1car s2car)
             (stream-cons s2car (merge s1 (stream-cdr s2)))]
          [else
             (stream-cons s1car (merge (stream-cdr s1) (stream-cdr s2)))]))]))

((lambda ()
(define s1
  (stream-cons
    1
    (merge
      (scale-stream s1 2)
      (merge
        (scale-stream s1 3)
        (scale-stream s1 5)))))

(mlog "ex-3.56"
  (stream->flist s1 15)  ; => (1 2 3 4 5 6 8 9 10 12 15 16 18 20 24)
)
))


;;; 3.57
((lambda ()
(define count 0)

(define (add-streams-with-counter s1 s2)
  (set! count (+ count 1))
  (stream-cons
    (+ (stream-car s1) (stream-car s2))
    (add-streams-with-counter (stream-cdr s1) (stream-cdr s2))))

(define fibs
  (stream* 0 1 (add-streams-with-counter (stream-cdr fibs) fibs)))

(mlog "ex-3.57"
  (stream-ref fibs 10)  ; => 55
  count  ; => 9 (n - 1)
)
))


;;; 3.58
((lambda ()
(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

(define s1 (expand 1 7 10))
(define s2 (expand 3 8 10))

(mlog "ex-3.58"
  (stream->flist s1 10)  ; => (1 4 2 8 5 7 1 4 2 8)
  (stream->flist s2 10)  ; => (3 7 5 0 0 0 0 0 0 0)
)
))


;;; 3.59
(define (integrate-series s)
  (let iter ([s s] [n 1])
    (define ns
      (stream-cons
        (/ (stream-car s) n)
        (iter (stream-cdr s) (+ n 1))))
    ns))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cos-series 
  (stream-cons 1 (scale-stream (integrate-series sin-series) -1)))

(define sin-series
  (stream-cons 0 (integrate-series cos-series)))

((lambda ()
(define s1 (integrate-series integers))

(mlog "ex-3.59"
  (stream->flist s1 5)  ; => (1 1 1 1 1)
  (stream->flist exp-series 10)
  (stream->flist sin-series 10)
  (stream->flist cos-series 10)
)
))


;;; 3.60
(define (add-series s1 s2)
  (add-streams s1 s2))

(define (mul-series s1 s2)
  (stream-cons
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (scale-stream (stream-cdr s1) (stream-car s2))
      (scale-stream (stream-cdr s2) (stream-car s1))
      (stream-cons 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

((lambda ()
(define s1
  (add-series
    (mul-series sin-series sin-series)
    (mul-series cos-series cos-series)))

(mlog "ex-3.60"
  (stream->flist s1 5)  ; => (1 0 0 0 0)
)
))


;;; 3.61
(define (reciprocal-series s)
  (let ([first (stream-car s)])
    (cond
      [(= first 0) (error 'reciprocal-series "Constant item is 0")]
      [else
        (define rs
          (stream-cons
            (/ 1 first)
            (scale-stream
              (mul-series (stream-cdr s) rs)
              (/ -1 first))))
        rs])))

((lambda ()
(define s1 exp-series)
(define s2 (reciprocal-series s1))

(mlog "ex-3.61"
  (stream->flist s1 5)
  (stream->flist s2 5)
  (stream->flist (mul-series s1 s2) 10)
)
))


;;; 3.62
(define (div-series s1 s2)
  (mul-series s1 (reciprocal-series s2)))

((lambda ()
(define tan-series (div-series sin-series cos-series))

(mlog "ex-3.62"
  (stream->flist sin-series 5)
  (stream->flist cos-series 5)
  (stream->flist tan-series 5)
)
))


;;; 3.63
; ignored


;;; 3.64
((lambda ()
(define (sqrt-import guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
      1.0
      (stream-map
        (lambda (g) (sqrt-import g x))
        guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)])
    (if (< (abs (- s0 s1)) tolerance)
      s1
      (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(mlog "ex-3.64"
  (stream->flist (sqrt-stream 2.0) 5)
  (sqrt 2.0 0.0001)
)
))


;;; 3.65
(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (stream-cons
      (- s2 (/ (sqr (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons
    s
    (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map
    stream-car
    (make-tableau transform s)))

((lambda ()
(define (ln2-summands n)
  (stream-cons
    (/ 1.0 n)
    (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(mlog "ex-3.65"
  (log 2)
  (stream->flist ln2-stream 10)
  (stream->flist (euler-transform ln2-stream) 10)
  (stream->flist (accelerated-sequence euler-transform ln2-stream) 10)
)
))


;;; 3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

((lambda ()
(define (ref-pos i j)
  (let ([head (- (expt 2 i) 2)]
        [delta (- j i)]
        [step (expt 2 i)])
    (cond
      [(= delta 0) head]
      [(= delta 1) (+ head (/ step 2))]
      [else (+ head (/ step 2) (* step (- delta 1)))])))

(define s1 (pairs integers integers))

(define (test i j)
  (define rp (ref-pos i j))
  (format "(~a, ~a) => ~a : ~a" i j rp (stream-ref s1 rp)))

(mlog "ex-3.66"
  (stream->flist s1 31)
  (test 1 100)
  (test 3 100)
  (test 6 10)
  (test 10 10)
  (ref-pos 100 100)
)
))


;;; 3.67
((lambda ()
(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (interleave
        (stream-map
          (lambda (x) (list x (stream-car t)))
          (stream-cdr s))
        (pairs (stream-cdr s) (stream-cdr t))))))

(define s1 (pairs integers integers))

(mlog "ex-3.67"
  (stream->flist s1 31)
)
))


;;; 3.68
; not work; infinite loop


;;; 3.69
((lambda ()
(define (triples s t u)
  (let ([s0 (stream-car s)]
        [sr (stream-cdr s)]
        [t0 (stream-car t)]
        [tr (stream-cdr t)]
        [u0 (stream-car u)]
        [ur (stream-cdr u)])
    (stream-cons
      (list s0 t0 u0)
      (interleave
        (stream-map
          (lambda (x) (list s0 u0 x))
          ur)
        (interleave
          (stream-map
            (lambda (x) (cons s0 x))
            (pairs tr ur))
          (triples sr tr ur))))))

(define s1 (triples integers integers integers))

(define s2
  (stream-filter
    (lambda (x)
      (let ([i (car x)] [j (cadr x)] [k (caddr x)])
        (= (+ (sqr i) (sqr j)) (sqr k))))
    s1))

(mlog "ex-3.69"
  (stream->flist s1 10)
  (stream->flist s2 2)
)
))


;;; 3.70
(define (merge-weighted s t weight)
  (cond
    [(stream-null? s) t]
    [(stream-null? t) s]
    [else
      (let* ([s0 (stream-car s)]
             [t0 (stream-car t)]
             [ws (weight s0)]
             [wt (weight t0)])
        (cond
          [(< ws wt) (stream-cons s0 (merge-weighted (stream-cdr s) t weight))]
          [(> ws wt) (stream-cons t0 (merge-weighted s (stream-cdr t) weight))]
          [else
            (stream* s0 t0 (merge-weighted (stream-cdr s) (stream-cdr t) weight))]))]))

(define (weighted-pairs s t weight)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

((lambda ()
(define s1
  (weighted-pairs
    integers
    integers
    (lambda (x)
      (+ (car x) (cadr x)))))

(define s2
  (stream-filter
    (lambda (x)
      (let ([i (car x)] [j (cadr x)])
        (or (= 0 (remainder i 2))
            (= 0 (remainder i 3))
            (= 0 (remainder i 5))
            (= 0 (remainder j 2))
            (= 0 (remainder j 3))
            (= 0 (remainder j 5)))))
    (weighted-pairs
      integers
      integers
      (lambda (x)
        (let ([i (car x)] [j (cadr x)])
          (+ (* 2 i) (* 3 j) (* 5 i j)))))))

(mlog "ex-3.70"
  (stream->flist s1 20)
  (stream->flist s2 20)
)
))


;;; 3.71 ~ 3.72
(define (streams-filtered-assoc pred assoc . streams)
  (let ([null-streams (filter stream-null? streams)])
    (if (> (length null-streams) 0)
      empty-stream
      (let ([heads (map stream-car streams)])
        (if (apply pred heads)
          (stream-cons
            (apply assoc heads)
            (apply streams-filtered-assoc (cons pred (cons assoc (map stream-cdr streams)))))
          (apply streams-filtered-assoc (cons pred (cons assoc (map stream-cdr streams)))))))))

((lambda ()
(define (cubic-sum p)
  (let ([i (car p)] [j (cadr p)])
    (+ (* i i i) (* j j j))))

(define (square-sum p)
  (let ([i (car p)] [j (cadr p)])
    (+ (* i i) (* j j))))

(define s1
  (weighted-pairs
    integers
    integers
    cubic-sum))

(define s2
  (stream-map
    (lambda (x) (cons (cubic-sum x) x))
    s1))

(define s3
  (streams-filtered-assoc
    (lambda (x1 x2)
      (= (cubic-sum x1) (cubic-sum x2)))
    (lambda (x1 x2)
      (list (cubic-sum x1) x1 x2))
    s1
    (stream-cdr s1)))

(define t1
  (weighted-pairs
    integers
    integers
    square-sum))

(define t2
  (streams-filtered-assoc
    (lambda (x1 x2 x3)
      (= (square-sum x1) (square-sum x2) (square-sum x3)))
    (lambda (x1 x2 x3)
      (list (square-sum x1) x1 x2 x3))
    t1
    (stream-cdr t1)
    (stream-cdr (stream-cdr t1))))

(mlog "ex-3.71"
  (stream->flist s1 20)
  (stream->flist s2 20)
  (stream->flist s3 6)
)

(mlog "ex-3.72"
  (stream->flist t2 10)
)
))


;;; 3.73
(define (integral integrand initial-value dt)
  (define int
    (stream-cons
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

((lambda ()
(define (RC r c dt)
  (lambda (v0 i)
    (add-streams
      (scale-stream i r)
      (integral
        (scale-stream i (/ 1.0 c))
        v0
        dt))))

(define RC1 (RC 5 1 0.5))

(define is1 (stream-cons 1 is1))

(mlog "ex-3.73"
  (stream->flist (RC1 0 is1) 20)
)
))


;;; 3.74
(define (sign-change-detector v1 v2)
  (cond
    [(and (< v1 0) (> v2 0)) -1]
    [(and (> v1 0) (< v2 0)) 1]
    [else 0]))

(define (make-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
    empty-stream
    (stream-cons
      (sign-change-detector
        (stream-car input-stream)
        last-value)
      (make-zero-crossings
        (stream-cdr input-stream)
        (stream-car input-stream)))))

((lambda ()
(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define zero-crossings-1 
  (make-zero-crossings sense-data 0))

(define zero-crossings-2
  (streams-map
    sign-change-detector
    sense-data
    (stream-cons 0 sense-data)))

(mlog "ex-3.74"
  (stream->list sense-data)
  (stream->list zero-crossings-1)
  (stream->list zero-crossings-2)
)
))


;;; 3.75
((lambda ()
(define (make-zero-crossings input-stream last-value last-avpt)
  (if (stream-null? input-stream)
    empty-stream
    (let* ([head (stream-car input-stream)]
           [avpt (average head last-value)])
      (stream-cons
        (sign-change-detector avpt last-avpt)
        (make-zero-crossings (stream-cdr input-stream) head avpt)))))

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(mlog "ex-3.75"
  (stream->list sense-data)
  (stream->list (make-zero-crossings sense-data 0 0))
)
))


;;; 3.76
((lambda ()
(define (smooth input-stream last-value)
  (if (stream-null? input-stream)
    empty-stream
    (stream-cons
      (/ (+ (stream-car input-stream) last-value) 2.0)
      (smooth (stream-cdr input-stream) (stream-car input-stream)))))

(define s1 (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(mlog "ex-3.76"
  (stream->list s1)
  (stream->list (smooth s1 0))
  (stream->list (make-zero-crossings (smooth s1 0) 0))
)
))


;;; 3.77
(define (integral-delayed
         delayed-integrand initial-value dt)
  (define int
    (stream-cons
     initial-value
     (let ((integrand 
            (force delayed-integrand)))
       (add-streams 
        (scale-stream integrand dt)
        int))))
  int)

(set! integral integral-delayed)

((lambda ()
(define (integral delayed-integrand initial-value dt)
  (stream-cons
    initial-value
    (let ([integrand (force delayed-integrand)])
      (if (stream-null? integrand)
        empty-stream
        (integral
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand)) initial-value)
          dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define ones
  (let ()
    (define s1 (stream-cons 1 (force (delay s2))))
    (define s2 s1)
    s1))

(define integers
  (let ()
    (define s1 (stream-cons 1 (force (delay s2))))
    (define s2 (stream-map inc s1))
    s1))

(define fibs
  (let ()
    (define s1 (stream-cons 1 (force (delay s2))))
    (define s2 (stream-cons 1 (force (delay s3))))
    (define s3 (add-streams s1 s2))
    s1))

(mlog "ex-3.77"
  (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
  (stream->flist ones 10)
  (stream->flist integers 10)
  (stream->flist fibs 10)
)
))


;;; 3.78
((lambda ()
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (add-streams
      (scale-stream y b)
      (scale-stream dy a)))
  y)

(mlog "ex-3.78"
  (stream-ref (solve-2nd 1 0 0.0001 1 1) 10000)  ; => 2.718 (e)
  (stream-ref (solve-2nd 0 -1 0.0001 1 0) 10472)  ; => 0.5 (cos 60)
  (stream-ref (solve-2nd 0 -1 0.0001 0 1) 5236)  ; => 0.5 (sin 30)
)
))


;;; 3.79
((lambda ()
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (streams-map f dy y))
  y)

(mlog "ex-3.79"
  (stream-ref (solve-2nd (lambda (dy/dt y) dy/dt) 0.0001 1 1) 10000)  ; => 2.718 (e)
  (stream-ref (solve-2nd (lambda (dy/dt y) (- y)) 0.0001 1 0) 10472)  ; => 0.5 (cos 60)
  (stream-ref (solve-2nd (lambda (dy/dt y) (- y)) 0.0001 0 1) 5236)  ; => 0.5 (cos 60)
)
))


;;; 3.80
((lambda ()
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 L))))
    (define dil
      (add-streams
        (scale-stream vc (/ 1 L))
        (scale-stream il (- (/ R L)))))
    (cons vc il)))

(define RLC1 (RLC 1 1 0.2 0.1))
(define rst (RLC1 0 10))

(mlog "ex-3.80"
  (stream->flist (car rst) 10)
  (stream->flist (cdr rst) 10)
)
))


;;; 3.81
((lambda ()
(define (rand-update x) (remainder (+ (* 13 x) 5) 24)) 
(define rand-init (rand-update (expt 2 32)))

(define (rand cmds)
  (let inner ([cmds cmds] [last rand-init])
    (if (stream-null? cmds)
      empty-stream
      (let* ([cmd (stream-car cmds)]
             [msg (car cmd)])
        (cond
          [(eqv? msg 'generate)
             (let ([number (rand-update last)])
               (stream-cons number (inner (stream-cdr cmds) number)))]
          [(eqv? msg 'reset)
             (let ([number (cadr cmd)])
               (stream-cons number (inner (stream-cdr cmds) number)))]
          [else (error 'rand "unknown message" msg)])))))

(mlog "ex3.81"
  (stream->list (rand (stream
    '(generate)
    '(generate)
    '(reset 100)
    '(generate)
    '(generate)
    '(generate)
    '(reset 200)
    '(generate)
    '(generate)
    '(generate)
    '(reset 100)
    '(generate)
    '(generate)
    '(generate)
    '(generate)
  )))
)
))


;;; 3.82
((lambda ()
(define (monte-carlo-stream trial-result-stream)
  (let inner ([s trial-result-stream] [trial 0] [passed 0])
    (if (stream-null? s)
      empty-stream
      (let* ([trial (+ trial 1)]
             [result (stream-car s)]
             [passed (if result (+ passed 1) passed)])
        (stream-cons
          (list trial passed (/ passed trial))
          (inner (stream-cdr s) trial passed))))))

(define (random-in-range low high)
  (let* ([segs 1000] [delta (/ (- high low) segs)])
    (exact->inexact (+ low (* delta (random (+ segs 1)))))))

(define (estimate-integral-stream pred x1 x2 y1 y2)
  (define (gen-random-stream)
    (stream-cons
      (cons (random-in-range x1 x2) (random-in-range y1 y2))
      (gen-random-stream)))

  (define random-stream (gen-random-stream))

  (define trial-result-stream (stream-map pred random-stream))

  (let* ([area (* (- x2 x1) (- y2 y1))])
    (stream-map
      (lambda (rst) (exact->inexact (* area (caddr rst))))
      (monte-carlo-stream trial-result-stream))))

(define s1
  (estimate-integral-stream
    (lambda (p) (<= (+ (sqr (car p)) (sqr (cdr p))) 1))
    -1 1 -1 1))

(mlog "ex-3.82"
  (stream->flist s1 10)
  (stream-ref s1 100)
  (stream-ref s1 10000)
)
))