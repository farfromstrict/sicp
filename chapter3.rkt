#lang sicp
(#%require "lib/libs.rkt")
(#%require racket)


;;; ex 3.1
(ex 1
(define (make-accumulator/v1 init)
  (let ([num init])
    (lambda (adder)
      (set! num (+ num adder))
      num)))

(define ((make-accumulator/v2 num) adder)
  (set! num (+ num adder))
  num)

(define make-accumulator make-accumulator/v2)

(define A (make-accumulator 5))

(?== 15 (A 10))
(?== 25 (A 10))
)


;;; ex 3.2
(ex 2
(define (make-monitored f)
  (let ([calls 0])
    (lambda (m)
      (cond
        [(eq? m 'how-many-calls?) calls]
        [else
         (set! calls (+ 1 calls))
         (f m)]))))

(define s (make-monitored sqrt))

(?== 10 (s 100))
(?== 1 (s 'how-many-calls?))
)


;;; ex 3.3
(ex 3
(define (make-account balance account-password)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch input-password m)
    (if (eq? input-password account-password)
        (cond
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit])
        incorrect-password))
  dispatch)
------
(define acc (make-account 100 'secret-password))

(?== 60 ((acc 'secret-password 'withdraw) 40))
(?== "Incorrect password" ((acc 'some-other-password 'deposit) 50))
)


;;; ex 3.4
(ex 4
(define (call-the-cops) "call-the-cops")

(define (make-account balance account-password)
  (define incorrect-times 0)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    (set! incorrect-times (+ 1 incorrect-times))
    (if (< incorrect-times 7)
        "Incorrect password"
        (call-the-cops)))
  (define (dispatch input-password m)
    (if (eq? input-password account-password)
        (cond
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit])
        incorrect-password))
  dispatch)

(define acc (make-account 100 'secret-password))

(?== 60 ((acc 'secret-password 'withdraw) 40))

(for ([i (range 6)])
  (?== "Incorrect password" ((acc 'some-other-password 'withdraw) 40)))
(?== "call-the-cops" ((acc 'some-other-password 'withdraw) 40))
)


;;; ex 3.5
(ex 5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0)
       (/ trials-passed trials)]
      [(experiment)
       (iter (- trials-remaining 1) (+ trials-passed 1))]
      [else
       (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))
------
(define (estimate-integral pred? x1 x2 y1 y2 trials)
  (define (random-in-interval low high)
    (define count 1000)
    (define delta (/ (- high low) count))
    (+ low (* delta (random count))))
  (define (experiment)
    (let ([x (random-in-interval x1 x2)]
          [y (random-in-interval y1 y2)])
      (pred? x y)))
  (monte-carlo trials experiment))

(define (estimate-pi trials)
  (* 4.0
    (estimate-integral
      (lambda (x y)
        (<= (+ (* x x) (* y y)) 1))
      -1 1 -1 1 trials)))

(?~= 3.1416 (estimate-pi 1000000) 0.01)
)


;;; ex 3.6
(ex 6
(define ((random-update a c m) x)
  (modulo (+ (* a x) c) m))

(define (make-rand)
  (define dice 1)
  (define (helper m)
    (cond
      [(eq? m 'generate)
       (set! dice ((random-update 17 43 100) dice))
       dice]
      [(eq? m 'reset)
       (lambda (x)
         (set! dice x)
         dice)]))
  helper)

(define rand (make-rand))

((rand 'reset) 100)
(define a (rand 'generate))
(define b (rand 'generate))
((rand 'reset) 100)
(?== a (rand 'generate))
(?== b (rand 'generate))
)


;;; ex 3.7
(ex 7
(define (make-joint account old-password new-password)
  (lambda (input-password m)
    (if (eq? input-password new-password)
        (account old-password m)
        (lambda (amount) "Incorrect password"))))

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(?== 200 ((peter-acc 'open-sesame 'deposit) 100))
(?== "Incorrect password" ((paul-acc 'open-sesame 'withdraw) 50))
(?== 150 ((paul-acc 'rosebud 'withdraw) 50))
)


;;; ex 3.8
(ex 8
(define (make-f)
  (let ([status 1])
    (lambda (x)
      (cond
        [(= status 0) 0]
        [(= x 0)
         (set! status 0)
         0]
        [else x]))))

((lambda ()
  (define f (make-f))
  (?== 0 (+ (f 0) (f 1)))
))

((lambda ()
  (define f (make-f))
  (?== 1 (+ (f 1) (f 0)))
))
)


;;; ex 3.9
(ex 9
;; --- 递归版本 ---
;;[global: {factorial => |`(n) ...` . ->global|} `(factorial 6)`
;; [E1: ->global {n => 6}] `(* 6 (factorial 5))`
;; [E2: ->global {n => 5}] `(* 5 (factorial 4))`
;; [E3: ->global {n => 4}] `(* 4 (factorial 3))`
;; [E4: ->global {n => 3}] `(* 3 (factorial 2))`
;; [E5: ->global {n => 2}] `(* 2 (factorial 1))`
;; [E6: ->global {n => 1}] `1`
;; [E5: ->global {n => 2}] `2`
;; [E4: ->global {n => 3}] `6`
;; [E3: ->global {n => 4}] `24`
;; [E2: ->global {n => 5}] `120`
;; [E1: ->global {n => 6}] `720`
;;
;; --- 迭代版本 ---
;; [global: {factorial => |`(n) ...` . ->global|} {fact-iter => |`(product counter max-count) ...` . ->global|}] `(factorial 6)`
;; [E1: ->global {n => 6}] `(fact-iter 1 1 6)`
;; [E2: ->global {product => 1} {counter => 1} {max-count => 6}] `(fact-iter 1 2 6)`
;; [E3: ->global {product => 1} {counter => 2} {max-count => 6}] `(fact-iter 2 3 6)`
;; [E4: ->global {product => 2} {counter => 3} {max-count => 6}] `(fact-iter 6 4 6)`
;; [E5: ->global {product => 6} {counter => 4} {max-count => 6}] `(fact-iter 24 5 6)`
;; [E6: ->global {product => 24} {counter => 5} {max-count => 6}] `(fact-iter 120 6 6)`
;; [E7: ->global {product => 120} {counter => 6} {max-count => 6}] `(fact-iter 720 7 6)`
;; [E8: ->global {product => 720} {counter => 7} {max-count => 6}] `720`
)


;;; ex 3.10
(ex 10
;; [global: {make-withdraw => |`(initial-amount) ...` . ->global|}] `(define W1 (make-withdraw 100))`
;; [E1: ->global {initial-amount => 100}] `(let ([balance 100]) ...)`
;; [E1: ->global {initial-amount => 100}] `((lambda (balance) ...) 100)`
;; [E1: ->global {initial-amount => 100} {X1 => |`(balance) ...` . ->E1|}] `X1 100`
;; [E2: ->E1 {balance => 100}] `(lambda (amount) ...)`
;; [global: {make-withdraw => |`(initial-amount) ...` . ->global|} {W1 => |`(amount) ...` . ->E2|}] `(W1 50)`
;; [E3: ->E2 {amount => 50}] `(... balance amount ...)`
;; [E3: ->E2 {amount => 50}] `50`
;; [E2: ->E1 {balance => 50}]
;; [global: {make-withdraw => |`(initial-amount) ...` . ->global|} {W1 => |`(amount) ...` . ->E2|}] `(define W2 (make-withdraw 100))`
;; [E4: ->global {initial-amount => 100}] `let ([balance 100] ...)`
;; [E4: ->global {initial-amount => 100}] `((lambda (balance) ...) 100)`
;; [E4: ->global {initial-amount => 100} {X2 => |`(amount) ...` . ->E4|}] `X2 100`
;; [E5: ->E4 {balance => 100}] `(lambda (amount) ...)`
;; [global: {make-withdraw => |`(initial-amount) ...` . ->global|} {W1 => |`(amount) ...` . ->E2|} {W2 => |`(amount) ...` . ->E5|}]
)


;;; ex 3.11
(ex 11
;; [global: {make-account => |`(balance) ...` . ->global|}] `(define acc (make-account 50))`
;; [E1: ->global {balance => 50}] `(define (withdraw amount) ...)`
;; [E1: ->global {balance => 50} {withdraw => |`(amount) ...` . ->E1|}] `(define (deposit amount) ...)`
;; [E1: ->global {balance => 50} {withdraw => |`(amount) ...` . ->E1|} {deposit => |`(amount) ...` . ->E1|}]
;; [E1: ->global {balance => 50} {withdraw => |`(amount) ...` . ->E1|} {deposit => |`(amount) ...` . ->E1|} {dispatch => |`(m) ...` . ->E1|}] `(define acc deposit)`
;; [global: {make-account => |`(balance) ...` . ->global|} {acc => |`(m) ...` . ->E1|}] `((acc 'deposit) 40)`
;; [E2: ->E1 {m => 'deposit}] `(deposit 40)`
;; [E3: ->E1 {amount => 40}] `(... balance amount ...)`
;; [E1: ->global {balance => 90} {withdraw => |`(amount) ...` . ->E1|} {deposit => |`(amount) ...` . ->E1|} {dispatch => |`(m) ...` . ->E1|}]
;; [E3: ->E1 {amount => 40}] `90`
;; [global: {make-account => |`(balance) ...` . ->global|} {acc => dispatch}] `((acc 'withdraw) 30)`
;; [E4: ->E1 {m => 'withdraw}] `(withdraw 30)`
;; [E5: ->E1 {amount => 30}] `(... balance amount ...)`
;; [E1: ->global {balance => 60} {withdraw => |`(amount) ...` . ->E1|} {deposit => |`(amount) ...` . ->E1|} {dispatch => |`(m) ...` . ->E1|}]
;; [E5: ->E1 {amount => 30}] `60`
;; [global: {make-account => |`(balance) ...` . ->global|} {acc => dispatch}] `(define acc2 (make-account 50))`
;; [E6: ->global {balance => 50}] `...`
;; [E6: ->global {balance => 50} {withdraw => |`(amount) ...` . ->E6|} {deposit => |`(amount) ...` . ->E6|} {dispatch => |`(m) ...` . ->E6|}] `(define acc2 deposit)`
;; [global: {make-account => |`(balance) ...` . ->global|} {acc => |`(m) ...` . ->E1|} {acc2 => |`(m) ...` . ->E6|}]
)


;;; ex 3.12
(ex 12
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))
------
(define (append x y)
  (if (null? x)
      y
      (mcons (mcar x) (append (mcdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (mlist 'a 'b))
;; x --> [a|.] --> [b|/]
(define y (mlist 'c 'd))
;; y --> [c|.] --> [d|/]
(define z (append x y))
;; z --> [a|.] --> [b|.] --> [c|.] --> [d|/]
;;                            ^
;;                            |
;;                            y

(?== (mlist 'a 'b 'c 'd) z)
(?== (mlist 'b) (mcdr x))

(define w (append! x y))
;; w --> [a|.] --> [b|.] --> [c|.] --> [d|/]
;;        ^                   ^
;;        |                   |
;;        x                   y
(?== (mlist 'a 'b 'c 'd) w)
(?== (mlist 'b 'c 'd) (mcdr x))
)


;;; ex 3.13
(ex 13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
------
(define z (make-cycle (mlist 'a 'b 'c)))
;; z --> [a|.] --> [b|.] --> [c|.]
;;        ^                     |
;;        |                     |
;;        -----------------------
;; (last-pair z) => 无限循环
(@>> z)    ; #0=(a b c . #0#)
(@>> (mcdr z))    ; #0=(b c a . #0#)
)


;;; ex 3.14
(ex 14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ([temp (mcdr x)])
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define x (list->mlist '(a b c)))
;; reverse list
(?== (mlist 'c 'b 'a) (mystery x))
)


;;; ex 3.15
(ex 15
(define (set-to-wow! x)
  (set-car! (mcar x) 'wow)
  x)

(define x (mlist 'a 'b))
(define z1 (mcons x x))
(define z2 (mcons (mlist 'a 'b) (mlist 'a 'b)))

(?== (list->mlist '((wow b) wow b)) (set-to-wow! z1))
;; x  --> [wow|.] --> [b|/]
;;         ^ ^
;;         | |
;; z1 --> [.|.]
(?== (list->mlist '((wow b) a b)) (set-to-wow! z2))
;;        [wow|.] --> [b|/]
;;         ^
;;         |
;; z2 --> [.|.] --> [a|.] --> [b|/]
)


;;; ex 3.16
(ex 16
(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))

(define x1 (mcons 'a (mcons 'b (mcons 'c 'd))))
;; x1 --> [a|.] --> [b|.] --> [c|d]
(?== 3 (count-pairs x1))

(define y1 (mcons 'b 'c))
(define x2 (mcons (mcons 'a y1) y1))
;;        [a|.] --> [b|c]
;;         ^         ^
;;         |         |
;;         | ---------
;;         | |
;; x2 --> [.|.]
(?== 4 (count-pairs x2))

(define y2 (mcons 'a 'b))
(define y3 (mcons y2 y2))
(define x3 (mcons y3 y3))
;;        [a|b]
;;         ^ ^
;;         | |
;;        [.|.]
;;         ^ ^
;;         | |
;; x3 --> [.|.]
(?== 7 (count-pairs x3))


;; 无限循环
;; 构造过程见 3.13
;; x4 --> [a|.] --> [b|.] --> [c|.]
;;         ^                     |
;;         |                     |
;;         -----------------------
)


;;; ex 3.17
(ex 17
(define (count-pairs x)
  (define visited '())
  (cond
    [(null? x) 0]
    [(not (mpair? x)) 0]
    [else
     (let loop ([lst x])
       (cond
         [(null? lst) 0]
         [(not (mpair? lst)) 0]
         [(memq lst visited) 0]
         [else
          (set! visited (cons lst visited))
          (let ([head (mcar lst)] [tail (mcdr lst)])
            (+ 1 (loop head) (loop tail)))]))]))

(define x1 (mcons 'a (mcons 'b (mcons 'c 'd))))
(?== 3 (count-pairs x1))

(define y1 (mcons 'b 'c))
(define x2 (mcons (mcons 'a y1) y1))
(?== 3 (count-pairs x2))

(define y2 (mcons 'a 'b))
(define y3 (mcons y2 y2))
(define x3 (mcons y3 y3))
(?== 3 (count-pairs x3))

(define x4 (make-cycle (mlist 'a 'b 'c)))
(?== 3 (count-pairs x4))
)


;;; ex 3.18
(ex 18
(define (cycle? lst)
  (let loop ([lst lst] [visited '()])
    (cond
      [(null? lst) #f]
      [(not (mpair? lst)) #f]
      [(memq (mcdr lst) visited) #t]
      [else (loop (mcdr lst) (cons (mcdr lst) visited))])))

(define x (make-cycle (mlist 'a 'b 'c)))
(define y (mcons 'd x))
(define z (mcons x 'd))
(?true (cycle? x))
(?true (cycle? y))
(?false (cycle? z))
)


;;; ex 3.19
(ex 19
(define (cycle? lst)
  (cond
    [(null? lst) #f]
    [(not (mpair? lst)) #f]
    [(null? (mcdr lst)) #f]
    [(not (mpair? (mcdr lst))) #f]
    [else
     (let loop ([t (mcdr lst)] [r (mcdr (mcdr lst))])
       (cond
         [(eq? t r) #t]
         [else (loop (mcdr t) (mcdr (mcdr r)))]))]))

(define x (make-cycle (mlist 'a 'b 'c)))
(define y (mcons 'd x))
(define z (mcons x 'd))
(?true (cycle? x))
(?true (cycle? y))
(?false (cycle? z))
)


;;; ex 3.20
(ex 20
;; [global: ] `(define x (cons 1 2))`
;; [E1: ->global {_ => '(1 2)}] `(cons _)`
;; [E1: ->global {_ => '(1 2)}] `(1 . 2)`
;; [global: {x => (1 . 2)}] `(define z (cons x x))`
;; [E2: ->global {_ => '(x x)}] `(cons _)`
;; [E2: ->global {_ => '(x x)}] `(x . x)`
;; [global: {x => (1 . 2)} {z => (x . x)}] `(set-car! (cdr z) 17)`
;; [E3: ->global {_ => '((cdr z) 17)}] `(set-car! _)`
;; [E4: ->E3 {_ => z}] `(cdr _)`
;; [E4: ->E3 {_ => z}] `x`
;; [E3: ->global {_ => '(x 17)}] `(set-car! _)`
;; [global: {x => (17 . 2)} {z => (x . x)}] `(car x)`
;; [E5: ->global {_ => x}] `(car x)`
;; [E5: ->global] `17`
;; [global: {x => (17 . 2)} {z => (x . x)}] `17`
(define x (mcons 1 2))
(define z (mcons x x))
(set-car! (mcdr z) 17)
(?== 17 (mcar x))
)


;;; ex 3.21
(ex 21
(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error 'queue "FRONT called with an empty queue: ~a" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (mcons item '())])
    (cond
      [(empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue]
      [else
       (set-cdr! (rear-ptr queue) new-pair)
       (set-rear-ptr! queue new-pair)
       queue])))

(define (delete-queue! queue)
  (cond
    [(empty-queue? queue)
     (error 'queue "DELETE! called with an empty queue: ~a" queue)]
    [else
     (set-front-ptr! queue (mcdr (front-ptr queue)))
     queue]))

(define (queue->list queue)
  (cond
    [(empty-queue? queue) '()]
    [else
     (let loop ([curr (front-ptr queue)] [rear (rear-ptr queue)] [rst '()])
       (cond
         [(eq? curr rear) (append rst (list (mcar curr)))]
         [else (loop (mcdr curr) rear (append rst (list (mcar curr))))]))]))

(define (print-queue queue)
  (printf ">(~a)\n" (join (queue->list queue) " ")))

(define q1 (make-queue))
(?true (empty-queue? q1))
(insert-queue! q1 'a)
(?false (empty-queue? q1))
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(?true (empty-queue? q1))
)


;;; ex 3.22
(ex 22
(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (insert! item)
      (let ([new-pair (mcons item '())])
        (cond
          [(null? front-ptr)
           (set! front-ptr new-pair)
           (set! rear-ptr new-pair)]
          [else
           (set-cdr! rear-ptr new-pair)
           (set! rear-ptr new-pair)])))
    (define (delete!)
      (cond
        [(null? front-ptr)
         (error 'queue "DELETE! an empty queue")]
        [else
         (set! front-ptr (mcdr front-ptr))]))
    (define (print)
      (printf ">~a\n" front-ptr))
    (define (dispatch m)
      (cond
        [(eq? m 'empty?) (null? front-ptr)]
        [(eq? m 'insert!) insert!]
        [(eq? m 'delete!) (delete!)]
        [(eq? m 'print) (print)]))
    dispatch))

(define q (make-queue))
(?true (q 'empty?))
((q 'insert!) 'a)
(?false (q 'empty?))
(q 'print)
((q 'insert!) 'b)
(q 'print)
(q 'delete!)
(q 'print)
(q 'delete!)
(?true (q 'empty?))
(q 'print)
)


;;; ex 3.23
(ex 23
(define (make-deque) (mcons '() '()))

(define (front-ptr deque) (mcar deque))

(define (rear-ptr deque) (mcdr deque))

(define (set-front-ptr! deque node) (set-car! deque node))

(define (set-rear-ptr! deque node) (set-cdr! deque node))

(define (build-node item) (mcons item (mcons '() '())))

(define (node-value node) (mcar node))

(define (prev-node node) (mcar (mcdr node)))

(define (next-node node) (mcdr (mcdr node)))

(define (set-prev-node! node prev) (set-car! (mcdr node) prev))

(define (set-next-node! node next) (set-cdr! (mcdr node) next))

(define (front-deque deque) (node-value (front-ptr deque)))

(define (rear-deque deque) (node-value (rear-ptr deque)))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (init-deque deque node)
  (set-front-ptr! deque node)
  (set-rear-ptr! deque node))

(define (clear-deque deque) (init-deque deque '()))

(define (bind-nodes prev next)
  (set-prev-node! next prev)
  (set-next-node! prev next))

(define (front-insert-deque! deque item)
  (let ([new-node (build-node item)])
    (cond
      [(empty-deque? deque) (init-deque deque new-node)]
      [else
       (bind-nodes new-node (front-ptr deque))
       (set-front-ptr! deque new-node)])
    deque))

(define (rear-insert-deque! deque item)
  (let ([new-node (build-node item)])
    (cond
      [(empty-deque? deque) (init-deque deque new-node)]
      [else
       (bind-nodes (rear-ptr deque) new-node)
       (set-rear-ptr! deque new-node)])
    deque))

(define (front-delete-deque! deque)
  (cond
    [(empty-deque? deque) (error 'deque "DELETE! an empty deque")]
    [(eq? (front-ptr deque) (rear-ptr deque)) (clear-deque deque)]
    [else (set-front-ptr! deque (next-node (front-ptr deque)))])
  deque)

(define (rear-delete-deque! deque)
  (cond
    [(empty-deque? deque) (error 'deque "DELETE! an empty deque")]
    [(eq? (front-ptr deque) (rear-ptr deque)) (clear-deque deque)]
    [else (set-rear-ptr! deque (prev-node (rear-ptr deque)))])
  deque)

(define (deque->list deque)
  (cond
    [(empty-deque? deque) '()]
    [else
     (let loop ([front (front-ptr deque)] [node (rear-ptr deque)] [rst '()])
       (cond
         [(eq? node front) (cons (node-value node) rst)]
         [else (loop front (prev-node node) (cons (node-value node) rst))]))]))

(define dq (make-deque))
(?true (empty-deque? dq))
(front-insert-deque! dq 'a)
(?== '(a) (deque->list dq))
(rear-insert-deque! dq 'b)
(?== '(a b) (deque->list dq))
(front-insert-deque! dq 'c)
(?== '(c a b) (deque->list dq))
(rear-delete-deque! dq)
(?== '(c a) (deque->list dq))
(front-delete-deque! dq)
(?== '(a) (deque->list dq))
(rear-delete-deque! dq)
(?true (empty-deque? dq))
)


(run-ex 23)