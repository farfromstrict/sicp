#lang sicp
(#%require "lib/libs.rkt")
(#%require "lib/stream.rkt")
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

;; (+ (f 0) (f 1))

; 从左向右
(let ([f (make-f)])
  (let ([a (f 0)])
    (let ([b (f 1)])
      (?== 0 (+ a b)))))

; 从右向左
(let ([f (make-f)])
  (let ([a (f 1)])
    (let ([b (f 0)])
      (?== 1 (+ a b)))))
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
(@>> z)    ;   #0=(a b c .   #0#)
(@>> (mcdr z))    ;   #0=(b c a .   #0#)
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
------
(define (queue->list queue)
  (cond
    [(empty-queue? queue) '()]
    [else
     (let loop ([curr (front-ptr queue)] [rear (rear-ptr queue)] [rst '()])
       (cond
         [(eq? curr rear) (append rst (list (mcar curr)))]
         [else (loop (mcdr curr) rear (append rst (list (mcar curr))))]))]))

(define (print-queue queue)
  (printf ">(~a)\n" (string-join (map symbol->string (queue->list queue)) " ")))

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


;;; ex 3.24
(ex 24
(define (make-table same-key?)
  (let ([table (mcons '*table* '())])
    (define (assoc key)
      (let loop ([t (mcdr table)])
        (cond
          [(null? t) #f]
          [(same-key? (mcar (mcar t)) key) (mcar t)]
          [else (loop (mcdr t))])))
    (define (lookup key)
      (let ([node (assoc key)])
        (if node
            (mcdr node)
            #f)))
    (define (insert! key value)
      (let ([node (assoc key)])
        (if node
            (set-cdr! node value)
            (let ([new-node (mcons (mcons key value) (mcdr table))])
              (set-cdr! table new-node)))))
    (define (->list)
      (let loop ([t table] [rst '()])
        (cond
          [(null? (mcdr t)) rst]
          [else (loop (mcdr t) (append rst (list (mcar (mcdr t)))))])))
    (define (dispatch m)
      (cond
        [(eq? m 'lookup) lookup]
        [(eq? m 'insert!) insert!]
        [(eq? m '->list) (->list)]))
    dispatch))

(define table (make-table (lambda (k1 k2) (< (abs (- k1 k2)) 0.1))))
((table 'insert!) 10 10)
((table 'insert!) 20 20)
((table 'insert!) 30 30)
(?== 20 ((table 'lookup) 19.98))
(?== 30 ((table 'lookup) 30.08))
(?false ((table 'lookup) 20.2))
)


;;; ex 3.25
(ex 25
(define (make-table)
  (define TABLE_HEADER '*table*)
  (define VALUE_FLAG '*value*)
  (define (cons-table cell) (mcons TABLE_HEADER (mcons cell '())))
  (define (init-table) (mcons TABLE_HEADER '()))
  (define (table-header t) (mcar t))
  (define (table-cells t) (mcdr t))
  (define (table? t) (and (mpair? t) (eq? TABLE_HEADER (table-header t))))
  (define (empty? t) (null? (table-cells t)))
  (define (build-cell key data subs) (mcons (mcons key data) (or subs (init-table))))
  (define (cell-key cell) (mcar (mcar cell)))
  (define (cell-data cell) (mcdr (mcar cell)))
  (define (cell-subs cell) (mcdr cell))
  (define (build-cell-data value) (mcons VALUE_FLAG value))
  (define (cell-has-value? cell) (and (mpair? cell) (mpair? (cell-data cell)) (eq? VALUE_FLAG (mcar (cell-data cell)))))
  (define (set-cell-subs! cell t) (set-cdr! cell t))
  (define (cell-value cell) (mcdr (cell-data cell)))
  (define (set-cell-value! cell value) (set-cdr! (mcar cell) (build-cell-data value)))
  (define full-table (init-table))
  (define (assoc-key t key)
    (cond
      [(not (table? t)) #f]
      [else
       (let loop ([cells (table-cells t)])
         (cond
           [(null? cells) #f]
           [(eq? key (cell-key (mcar cells))) (mcar cells)]
           [else (loop (mcdr cells))]))]))
  (define (assoc-keys t keys)
    (cond
      [(null? keys) #f]
      [(null? t) #f]
      [(null? (cdr keys)) (assoc-key t (car keys))]
      [else
       (let ([cell (assoc-key t (car keys))])
         (cond
           [(not cell) #f]
           [else (assoc-keys (cell-subs cell) (cdr keys))]))]))
  (define (lookup keys)
    (let ([cell (assoc-keys full-table keys)])
      (cond
        [(not cell) #f]
        [(cell-has-value? cell) (cell-value cell)]
        [else #f])))
  (define (build-table keys value)
    (let loop ([keys (reverse keys)] [start #t] [rst #f])
      (cond
        [(null? keys) rst]
        [else
         (let ([first-key (car keys)] [rest-keys (cdr keys)])
           (loop rest-keys #f (cons-table (build-cell first-key (if start (build-cell-data value) '()) rst))))])))
  (define (insert-cell! t cell)
    (set-cdr! t (mcons cell (table-cells t))))
  (define (insert-keys-value! t keys value)
    (cond
      [(null? keys) #f]
      [(null? (cdr keys))
       (let ([cell (assoc-key t (car keys))])
         (cond
           [cell (set-cell-value! cell value)]
           [else (insert-cell! t (build-cell (car keys) (build-cell-data value) #f))]))]
      [else
       (let ([first-key (car keys)] [rest-keys (cdr keys)])
         (let ([cell (assoc-key t first-key)])
           (cond
             [(not cell)
              (insert-cell! t (build-cell first-key '() (build-table rest-keys value)))]
             [(null? (cell-subs cell))
              (set-cell-subs! cell (build-table rest-keys value))]
             [else (insert-keys-value! (cell-subs cell) rest-keys value)])))]))
  (define (insert! keys value)
    (insert-keys-value! full-table keys value))
  (define (print-table t n)
    (define (n-space n)
      (let loop ([t n] [rst ""])
        (cond
          [(= t 0) rst]
          [else (loop (- t 1) (format "~a~a" rst " "))])))
    (define spaces (n-space n))
    (define (print-cell cell)
      (let ([key (cell-key cell)] [data (cell-data cell)] [subs (cell-subs cell)])
        (printf "~a- [~a : ~a]\n" spaces key (if (null? data) "" (mcdr data)))
        (print-table subs (+ n 2))))
    (printf "~a*table*\n" spaces)
    (cond
      [(or (null? t) (empty? t)) (void)]
      [else
       (let loop ([cells (table-cells t)])
         (cond
           [(null? cells) (void)]
           [else (print-cell (mcar cells)) (loop (mcdr cells))]))]))
  (define (dispatch m)
    (cond
      [(eq? m 'raw) full-table]
      [(eq? m 'empty?) (empty? full-table)]
      [(eq? m 'print) (print-table full-table 0)]
      [(eq? m 'insert!) insert!]
      [(eq? m 'lookup) lookup]))
  dispatch)

(define table (make-table))
(?true (table 'empty?))
(?false ((table 'lookup) '(a b c)))
((table 'insert!) '(a b c) 10)
(?false (table 'empty?))
((table 'insert!) '(a b) 20)
((table 'insert!) '(d) 30)
((table 'insert!) '(a b e) 40)
(?== 10 ((table 'lookup) '(a b c)))
(?== 20 ((table 'lookup) '(a b)))
(?== 30 ((table 'lookup) '(d)))
(?== 40 ((table 'lookup) '(a b e)))
(?false ((table 'lookup) '(a)))
(?false ((table 'lookup) '()))
(?false ((table 'lookup) '(e)))
; (table 'print)
)


;;; ex 3.26
(ex 26
(define (make-table)
  (define (make-tree entry lb rb) (mcons entry (mcons lb rb)))
  (define (make-entry key value) (mcons key value))
  (define (entry tree) (mcar tree))
  (define (entry-key tree) (mcar (entry tree)))
  (define (entry-value tree) (mcdr (entry tree)))
  (define (left-branch tree) (mcar (mcdr tree)))
  (define (right-branch tree) (mcdr (mcdr tree)))
  (define (set-entry-value! tree value) (set-cdr! (entry tree) value))
  (define (set-left-branch! tree branch) (set-car! (mcdr tree) branch))
  (define (set-right-branch! tree branch) (set-cdr! (mcdr tree) branch))
  (define (empty? tree) (null? tree))
  (define (lookup key tree)
    (cond
      [(empty? tree) #f]
      [(= key (entry-key tree)) (entry-value tree)]
      [(< key (entry-key tree)) (lookup key (left-branch tree))]
      [else (lookup key (right-branch tree))]))
  (define (insert! key value tree)
    (cond
      [(null? tree) (void)]
      [(= key (entry-key tree)) (set-entry-value! tree value)]
      [(< key (entry-key tree))
       (let ([branch (left-branch tree)])
         (cond
           [(null? branch) (set-left-branch! tree (make-tree (make-entry key value) '() '()))]
           [else (insert! key value branch)]))]
      [else
       (let ([branch (right-branch tree)])
         (cond
           [(null? branch) (set-right-branch! tree (make-tree (make-entry key value) '() '()))]
           [else (insert! key value branch)]))]))
  (define table-tree '())
  (define (dispatch m)
    (cond
      [(eq? m 'empty?) (empty? table-tree)]
      [(eq? m 'insert!)
       (lambda (key value)
         (cond
           [(null? table-tree) (set! table-tree (make-tree (make-entry key value) '() '()))]
           [else (insert! key value table-tree)]))]
      [(eq? m 'lookup) (lambda (key) (lookup key table-tree))]
      [(eq? m 'print) (printf "~a\n" table-tree)]))
  dispatch)

(define table (make-table))
(?true (table 'empty?))
(?false ((table 'lookup) 'a))
(for ([k (string->list "helloworld")]
      [v '(10 20 30 40 50 60 70 80 90 100)])
  ((table 'insert!) (char->integer k) v))
(?false (table 'empty?))
(?== 10 ((table 'lookup) (char->integer #\h)))
(?== 20 ((table 'lookup) (char->integer #\e)))
(?== 90 ((table 'lookup) (char->integer #\l)))
(?== 70 ((table 'lookup) (char->integer #\o)))
(?== 60 ((table 'lookup) (char->integer #\w)))
(?== 80 ((table 'lookup) (char->integer #\r)))
(?== 100 ((table 'lookup) (char->integer #\d)))
)


;;; ex 3.27
(ex 27
;;   #000 [global: {fib => |`(n)...` . ->global|} {memoize => |`(f)...` . ->global|}] `(define memo-fib (memoize ...))` 
;;   #001 [global: {fib} {memoize}] `(memoize (lambda (n) ...))`
;;   #002 [E1: ->global {f => (lambda (n) ...)}] `(memoize ...)`
;;   #003 [E1: ->global {f => ...}] `(lambda (n) ...)`
;;   #004 [E1: ->global {f => |`(n) (cond ...)` . ->E1|}] `(memoize ...)`
;;   #005 [E1: ->global {f}] `(let ([table (make-table)]) ...)`
;;   #006 [E1: ->global {f}] `((lambda (table)...) (make-table))`
;;   #007 [E1: ->global {f} {X1 => |`(table)...` . ->E1|}] `X1 (make-table)`
;;   #008 [E2: ->E1 {table => '()}] `(lambda (x) ...)`
;;   #009 [E2: ->E1 {table} {X2 => |`(x)...` . ->E2|}] `X2`
;;   #010 [global: {fib} {memoize}] `(define memo-fib X2)`
;;   #011 [global: {fib} {memoize} {memo-fib => |`(x) (let ([pcr ...]) ...)` . ->E2|}] `(memo-fib 3)`
;;   #012 [E3: ->E2 {x => 3}] `(let ([pcr ...]) ...)`
;;   #013 [E3: ->E2 {x => 3}] `((lambda (pcr) ...) (lookup x table))`
;;   #014 [E3: ->E2 {x => 3}] `((lambda (pcr) ...) (lookup 3 '()))`
;;   #015 [E3: ->E2 {x => 3} {X3 => |`(pcr)...` . ->E3|}] `(X3 #f)`
;;   #016 [E4: ->E3 {pcr => #f}] `(or #f ...)`
;;   #017 [E4: ->E3 {pcr}] `(let ([result (f x)]) ...)`
;;   #018 [E4: ->E3 {pcr}] `((lambda (result) ...) (f x))`
;;   #019 [E4: ->E3 {pcr} {X4 => |`(result)...` . ->E4|}] `(X4 (f 3))`
;; * #020 [E4: ->E3 {pcr} {X4 => |`(result)...` . ->E4|}] `(f 3)`
;;   #021 [E5: ->E1 {n => 3}] `(cond ...)`
;;   #022 [E5: ->E1 {n => 3}] `(+ (memo-fib 2) (memo-fib 1))`
;;   #023 [E5: ->E1 {n => 3}] `(memo-fib 2)`
;;   #024 [E6: ->E2 {x => 2}] `(let ([pcr ...]) ...)`
;;   #025 [E6: ->E2 {x => 2}] `((lambda (pcr) ...) (lookup 2 '())))`
;;   #026 [E6: ->E2 {x => 2} {X5 => |`(pcr)...` . ->E6|}] `(X5 #f)`
;;   #027 [E7: ->E6 {pcr => #f}] `(or #f ...)`
;;   #028 [E7: ->E6 {pcr}] `(let ([result (f x)]) ...)`
;;   #029 [E7: ->E6 {pcr}] `((lambda (result) ...) (f x))`
;;   #030 [E7: ->E6 {pcr} {X6 => |`(result)...` . ->E7|}] `(X6 (f 2))`
;; * #031 [E7: ->E6 {pcr} {X6 => |`(result)...` . ->E7|}] `(f 2)`
;;   #032 [E8: ->E1 {n => 2}] `(cond ...)`
;;   #033 [E8: ->E1 {n => 2}] `(+ (memo-fib 1) (memo-fib 0))`
;;   #034 [E8: ->E1 {n => 2}] `(memo-fib 1)`
;;   #035 [E9: ->E2 {x => 1}] `(let ([pcr ...]) ...)`
;;   #036 [E9: ->E2 {x => 1}] `((lambda (pcr) ...) (lookup 1 '()))`
;;   #037 [E9: ->E2 {x => 1} {X7 => |`(pcr)...` . ->E9|}] `(X7 #f)`
;;   #038 [E10: ->E9 {pcr => #f}] `(or #f ...)`
;;   #039 [E10: ->E9 {pcr}] `(let ([result (f x)]) ...)`
;;   #040 [E10: ->E9 {pcr}] `((lambda (result) ...) (f x))`
;;   #041 [E10: ->E9 {pcr} {X8 => |`(result)...` . ->E10|}] `(X8 (f 1))`
;; * #042 [E10: ->E9 {pcr} {X8 => |`(result)...` . ->E10|}] `(f 1)`
;;   #043 [E11: ->E1 {n => 1}] `(cond ...)`
;;   #044 [E11: ->E1 {n => 1}] `1`
;;   #045 [E10: ->E9 {pcr} {X8}] `(X8 1)`
;;   #046 [E11: ->E10 {result => 1}] `(insert! x result table) result`
;;   #047 [E11: ->E10 {result => 1}] `(insert! 1 1 table) 1`
;;   #048 [E2: ->E1 {table => '((1 1))}]
;;   #049 [E11: ->E10 {result => 1}] `1`
;;   #050 [E8: ->E1 {n => 2}] `(+ 1 (memo-fib 0))`
;;   #051 [E8: ->E1 {n => 2}] `(memo-fib 0)`
;;   #052 [E12: ->E2 {x => 0}] `(let ([pcr ...]) ...)`
;;   #053 [E12: ->E2 {x => 0}] `((lambda (pcr) ...) (lookup 0 '((1 1))))`
;;   #054 [E12: ->E2 {x => 0} {X9 => |`(pcr)...` . ->E12|}] `(X9 #f)`
;;   #055 [E13: ->E12 {pcr => #f}] `(or #f ...)`
;;   #056 [E13: ->E12 {pcr}] `(let ([result (f x)]) ...)`
;;   #057 [E13: ->E12 {pcr}] `((lambda (result) ...) (f x))`
;;   #058 [E13: ->E12 {pcr} {X10 => |`(result)...` . ->E13|}] `(X10 (f 0))`
;; * #059 [E13: ->E12 {pcr} {X10 => |`(result)...` . ->E13|}] `(f 0)`
;;   #060 [E14: ->E1 {n => 0}] `(cond ...)`
;;   #061 [E14: ->E1 {n => 0}] `1`
;;   #062 [E13: ->E12 {pcr} {X10}] `(X10 1)`
;;   #063 [E14: ->E13 {result => 1}] `(insert! x result table) result`
;;   #064 [E14: ->E13 {result => 1}] `(insert! 0 1 table) 1`
;;   #065 [E2: ->E1 {table => '((1 1) (0 1))}]
;;   #066 [E14: ->E13 {result => 0}] `1`
;;   #067 [E8: ->E1 {n => 2}] `(+ 1 1)`
;;   #068 [E8: ->E1 {n => 2}] `2`
;;   #069 [E7: ->E6 {pcr} {X6 => |`(result)...` . ->E7|}] `(X6 2)`
;;   #069 [E15: ->E7 {result => 2}] `(insert! x result table) result`
;;   #070 [E15: ->E7 {result => 2}] `(insert! 2 2 table) 2`
;;   #071 [E2: ->E1 {table -> '((1 1) (0 1) (2 2))}]
;;   #072 [E15: ->E7 {result => 2}] `2`
;;   #073 [E5: ->E1 {n => 3}] `(+ 2 (memo-fib 1))`
;;   #074 [E5: ->E1 {n => 3}] `(memo-fib 1)`
;;   #075 [E16: ->E2 {x => 1}] `(let ([pcr ...]) ...)`
;;   #076 [E16: ->E2 {x => 1}] `((lambda (pcr) ...) (lookup 1 '((1 1) (0 1) (2 2))))`
;;   #077 [E16: ->E2 {x => 1} {X11 => |`(pcr)...` . ->E16|}] `(X11 1)`
;;   #078 [E17: ->E16 {pcr => 1}] `(or 1 ...)`
;;   #079 [E17: ->E16 {pcr => 1}] `1`
;;   #080 [E5: ->E1 {n => 3}] `(+ 2 1)`
;;   #081 [E5: ->E1 {n => 3}] `3`
;;   #082 [E4: ->E3 {pcr} {X4 => |`(result)...` . ->E4|}] `(X4 3)`
;;   #083 [E18: ->E4 {result => 3}] `(insert! x result table) result`
;;   #084 [E18: ->E4 {result => 3}] `(insert! 3 3 table) 3`
;;   #085 [E2: ->E1 {table => '((1 1) (0 1) (2 2) (3 3))}]
;;   #086 [E18: ->E4 {result => 3}] `3`
;;   #087 [global: {fib} {memoize} {memo-fib}] `3`

;; 关键步骤 (f x) 调用次数 4 次 = n + 1
;;
;; (define memo-fib (memoize fib)) 不符合预期, (f x) => (fib x) 仅在fib函数中递归, table不起作用
)


;;; ex 3.28
(ex 28
(define (make-time-segment time queue) (mcons time queue))

(define (segment-time s) (mcar s))

(define (segment-queue s) (mcdr s))

(define (make-agenda) (mcons 0 '()))

(define (current-time agenda) (mcar agenda))

(define (set-current-time! agenda time) (set-car! agenda time))

(define (segments agenda) (mcdr agenda))

(define (set-segments! agenda segments) (set-cdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))

(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (cond
      [(= time (segment-time (mcar segments)))
       (insert-queue! (segment-queue (mcar segments)) action)]
      [else
       (let ([rest (mcdr segments)])
         (cond
           [(belongs-before? rest)
            (set-cdr! segments (mcons (make-new-time-segment time action) (mcdr segments)))]
           [else (add-to-segments! rest)]))]))
  (let ([segments (segments agenda)])
    (cond
      [(belongs-before? segments)
       (set-segments! agenda (mcons (make-new-time-segment time action) segments))]
      [else (add-to-segments! segments)])))

(define (first-agenda-item agenda)
  (cond
    [(empty-agenda? agenda) (error 'first-agenda-time "Agenda is empty")]
    [else
     (let ([first-seg (first-segment agenda)])
       (set-current-time! agenda (segment-time first-seg))
       (front-queue (segment-queue first-seg)))]))

(define (remove-first-agenda-item! agenda)
  (let ([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (cond
      [(empty-queue? q)
       (set-segments! agenda (rest-segments agenda))])))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (propagate)
  (cond
    [(empty-agenda? the-agenda) 'done]
    [else
     (let ([first-item (first-agenda-item the-agenda)])
       (first-item)
       (remove-first-agenda-item! the-agenda)
       (propagate))]))

(define (make-wire)
  (let ([signal-value -1] [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (mcons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) signal-value]
        [(eq? m 'set-signal!) set-my-signal!]
        [(eq? m 'add-action!) accept-action-procedure!]
        [else (error 'make-wire "Unknown operation: ~a" m)]))
    dispatch))

(define (call-each procedures)
  (cond
    [(null? procedures) 'done]
    [else
     ((mcar procedures))
     (call-each (mcdr procedures))]))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

(define (logic-or s1 s2)
  (cond
    [(or (= s1 -1) (= s2 -1)) -1]
    [(= s1 1) 1]
    [(= s2 1) 1]
    [else 0]))

(define (probe name wire)
  (add-action!
    wire
    (lambda ()
      (let ([value (get-signal wire)] [time (current-time the-agenda)])
        (cond
          [(not (= value -1))
           (printf "[~a] ~a : ~a\n" time name value)])))))

(define or-gate-delay 5)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logic-or (get-signal a1) (get-signal a2))])
      (after-delay
        or-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
------
(set! the-agenda (make-agenda))
(define in1 (make-wire))
(define in2 (make-wire))
(define out (make-wire))
(probe 'in1 in1)
(probe 'in2 in2)
(probe 'out out)
(or-gate in1 in2 out)
(set-signal! in1 1)
(set-signal! in2 0)
(propagate)
)


;;; ex 3.29
(ex 29
(define (logic-not s)
  (cond
    [(= s -1) -1]
    [(= s 0) 1]
    [(= s 1) 0]))

(define (logic-and s1 s2)
  (cond
    [(or (= s1 -1) (= s2 -1)) -1]
    [(= s1 0) 0]
    [(= s2 0) 0]
    [else 1]))

(define inverter-delay 2)

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logic-not (get-signal input))])
      (after-delay
        inverter-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define and-gate-delay 3)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value (logic-and (get-signal a1) (get-signal a2))])
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
------
;; a ^ b = ~((~a) * (~b))
;; or-gate-delay = 2 * inverter-delay + and-gate-delay

(define (or-gate a1 a2 output)
  (let ([b1 (make-wire)]
        [b2 (make-wire)]
        [c1 (make-wire)])
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c1)
    (inverter c1 output)
    'ok))

(set! the-agenda (make-agenda))
(define in1 (make-wire))
(define in2 (make-wire))
(define out (make-wire))
(probe 'in1 in1)
(probe 'in2 in2)
(probe 'out out)
(or-gate in1 in2 out)
(set-signal! in1 1)
(set-signal! in2 0)
(propagate)
)


;;; ex 3.30
(ex 30
;; half-adder-delay:c = and-gate-delay = 3
;; half-adder-delay:s = (max (and-gate-delay + inverter-delay) or-gate-delay) + and-gate-delay = 8
;; half-adder-delay = 8
(define (half-adder a b s c)
  (let ([d (make-wire)] [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; full-adder-delay:sum = 2 * half-adder-delay:s = 16
;; full-adder-delay:c-out = half-adder-delay:s + half-adder-delay:c + or-gate-delay = 16
;; full-adder-delay = 16
(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)] [c1 (make-wire)] [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
------
(define (make-wires n) (build-list 5 (lambda (x) (make-wire))))

;; ripple-carry-adder-delay = n * full-adder-delay
(define (ripple-carry-adder An Bn Sn C)
  (define (ref lst x) (list-ref lst (- x 1)))
  (define n (length An))
  (define Cn (make-wires n))
  (for ([i (reverse (range 1 (+ n 1)))])
    (full-adder
      (ref An i)
      (ref Bn i)
      (ref Cn i)
      (ref Sn i)
      (if (= i 1) C (ref Cn (- i 1)))))
  (set-signal! (ref Cn n) 0)
  'ok)

(define (set-signals! wires signals)
  (for-each
    (lambda (wire signal) (set-signal! wire signal))
    wires signals))

(define (probes name wires)
  (for-each
    (lambda (wire)
      (add-action!
        wire
        (lambda ()
          (let ([signals (map get-signal wires)])
            (cond
              [(memq -1 signals) (void)]
              [else (printf "[~a] ~a : ~a\n" (current-time the-agenda) name signals)])))))
    wires))

(set! the-agenda (make-agenda))
(define n 5)
(define An (make-wires n))
(define Bn (make-wires n))
(define Sn (make-wires n))
(define C (make-wire))
(ripple-carry-adder An Bn Sn C)
(probes 'A An)
(probes 'B Bn)
(probes 'S Sn)
(probe 'C C)
(set-signals! An '(1 0 1 1 1))
(set-signals! Bn '(1 0 0 1 1))
(propagate)
)


;;; ex 3.31
(ex 31
(define (probe/v1 name wire)
  (add-action!
    wire
    (lambda ()
      (printf "[~a] ~a : ~a\n" (current-time the-agenda) name (get-signal wire)))))

(set the-agenda (make-agenda))
(define a (make-wire))
(define b (make-wire))
(define s (make-wire))
(define c (make-wire))
(probe/v1 'a a)
(probe/v1 'b b)
(probe/v1 's s)
(probe/v1 'c c)

(half-adder a b s c)
(set-signal! a 1)
(set-signal! b 1)
(propagate)

;; 如果不调用过程, 则动作不会被立即触发
;; 对于书中示例, 即probe调用时不会立即输出wire当前值(即初始值), 只有后续set-signal!才能触发输出
)


;;; ex 3.32
(ex 32
;; 保证先加入的动作先处理
(define (test bfirst predicate-value)
  (let ([a (make-wire)] [b (make-wire)] [out (make-wire)])
    (probe 'a a)
    (probe 'b b)
    (probe 'out out)
    (and-gate a b out)
    (set! the-agenda (make-agenda))
    (set-signal! a 0)
    (set-signal! b 1)  ; out --> 0
    (cond
      [bfirst
       (after-delay 5 (lambda () (set-signal! b 0)))  ; out --> 0
       (after-delay 5 (lambda () (set-signal! a 1)))  ; out --> 1
       (void)]
      [else
       (after-delay 5 (lambda () (set-signal! a 1)))  ; out --> 1
       (after-delay 5 (lambda () (set-signal! b 0)))  ; out --> 0
       (void)])
    (propagate)
    (?== predicate-value (get-signal out))))

(test #f 0)
(@>> "")
(test #t 0)
(@>> "----------")

;; FIFO, 输出信息变化为:
;; [0](0,1 -> 0) => [5](1,1 -> 1) => [5](1,0 -> 0)
;; [0](0,1 -> 0) => [5](0,0 -> 0) => [5](1,0 -> 0)
;; 最终结果都正确

(define (make-queue/lifo)
  (let ([queue (mcons '*lifo* '())])
    (define (empty?) (null? (mcdr queue)))
    (define (insert! item)
      (set-cdr! queue (mcons item (mcdr queue))))
    (define (delete!)
      (cond
        [(not (empty?)) (set-cdr! queue (mcdr (mcdr queue)))]))
    (define (dispatch m)
      (cond
        [(eq? m 'empty?) (empty?)]
        [(eq? m 'front) (mcar (mcdr queue))]
        [(eq? m 'insert!) insert!]
        [(eq? m 'delete!) (delete!)]
        [(eq? m 'print) (printf "~a\n" queue)]))
    dispatch))

(@update ([make-queue make-queue/lifo]
          [empty-queue? (lambda (q) (q 'empty?))]
          [front-queue (lambda (q) (q 'front))]
          [insert-queue! (lambda (q item) ((q 'insert!) item))]
          [delete-queue! (lambda (q) (q 'delete!))])
  (test #f 0)
  (@>> "")
  (test #t 1))

;; 关键点: and-gate 的 and-action-procedure 方法, 是先计算当前 output 的预期值, 再调用 after-delay 放入待处理表队列
;; LIFO, 输出信息变化为:
;;
;; <a先b后> [0](0,1 -> 0) => [5](1,1 -> 1) => [5](1,0 -> 0)
;; [5]时刻 队列为 (*lifo* (set-signal! b 0) (set-signal! a 1))
;; 先处理 b <- 0, 计算得output预期值为 0 * 0 = 0, 将 (set-signal! out 0) 放入 [8]时刻 队列
;; 再处理 a <- 1, 计算得output预期值为 1 * 0 = 0, 将 (set-signal! out 0) 放入 [8]时刻 队列
;; [8]时刻队列为 (*lifo* (set-signal! out 0) (set-signal! out 0))
;; 最终结果 out <- 0 **正确**
;;
;; <b先a后> [0](0,1 -> 0) => [5](0,0 -> 0) => [5](1,0 -> 0)
;; [5]时刻 队列为 (*lifo* (set-signal! a 1) (set-signal! b 0))
;; 先处理 a <- 1, 计算得output预期值为 1 * 1 = 1, 将 (set-signal! out 1) 放入 [8]时刻 队列
;; 再处理 b <- 0, 计算得output预期值为 1 * 0 = 0, 将 (set-signal! out 0) 放入 [8]时刻 队列
;; [8]时刻队列为 (*lifo* (set-signal! out 0) (set-signal! out 1))
;; 最终结果 out <- 1 **错误**
)


;;; ex 3.33
(ex 33
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond
      [(null? items) 'done]
      [(eq? (car items) exception) (loop (cdr items))]
      [else
       (procedure (car items))
       (loop (cdr items))]))
  (loop list))

(define (inform-about-value constraint) (constraint 'I-hava-a-value))

(define (inform-about-no-value constraint) (constraint 'I-lost-my-value))

(define (get-name me) (if (procedure? me) (me 'type) me))

(define (make-connector)
  (let ([value #f] [informant #f] [constraints '()] [name #f])
    (define (set-my-value newval setter)
      ;(printf "---[00] ~a, ~a(~a) -> ~a(~a), ~a\n" name value (get-name informant) newval (get-name setter) (map get-name constraints))
      (cond
        [(not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter inform-about-value constraints)]
        [(not (= value newval)) (error 'make-connector "Contradiction: [~a] ~a -> ~a (~a)" name value newval (get-name setter))]
        [else 'ignored]))
    (define (forget-my-value retractor)
      ;(printf "---[01] ~a, ~a -> ~a, ~a\n" name (get-name informant) (get-name retractor) (map get-name constraints))
      (if (eq? retractor informant)
          (begin
            (set! informant #f)
            (for-each-except retractor inform-about-no-value constraints))
          'ignored))
    (define (connect new-constraint)
      (unless
        (memq new-constraint constraints)
        (set! constraints (cons new-constraint constraints)))
      (when (has-value? me) (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        [(eq? request 'has-value?) (if informant #t #f)]
        [(eq? request 'value) value]
        [(eq? request 'set-value!) set-my-value]
        [(eq? request 'forget) forget-my-value]
        [(eq? request 'connect) connect]
        [(eq? request 'type) 'connector]
        [(eq? request 'name) (lambda (n) (set! name n))]
        [(eq? request 'informant) (get-name informant)]
        [else (error 'make-connect "Unknown request: ~a" request)]))
    me))

(define (has-value? connector) (connector 'has-value?))

(define (get-value connector) (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor) ((connector 'forget) retractor))

(define (connect connector new-constraint) ((connector 'connect) new-constraint))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      [(and (has-value? a1) (has-value? a2))
       (set-value!
         sum
         (+ (get-value a1) (get-value a2))
         me)]
      [(and (has-value? a1) (has-value? sum))
       (set-value!
         a2
         (- (get-value sum) (get-value a1))
         me)]
      [(and (has-value? a2) (has-value? sum))
       (set-value!
         a1
         (- (get-value sum) (get-value a2))
         me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-hava-a-value)
       (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [(eq? request 'type) 'adder]
      [else (error 'adder "Unknown request: ~a" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      [(or (and (has-value? m1) (= 0 (get-value m1)))
           (and (has-value? m2) (= 0 (get-value m2))))
       (set-value! product 0 me)]
      [(and (has-value? m1) (has-value? m2))
       (set-value!
         product
         (* (get-value m1) (get-value m2))
         me)]
      [(and (has-value? product) (has-value? m1))
       (set-value!
         m2 
         (/ (get-value product) (get-value m1))
         me)]
      [(and (has-value? product) (has-value? m2))
       (set-value!
         m1
         (/ (get-value product) (get-value m2))
         me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-hava-a-value)
       (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [(eq? request 'type) 'multiplier]
      [else (error 'multiplier "Unknown request: ~a" request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (cond
      [(eq? request 'type) 'constant]
      [else (error 'constant "Unknown request: ~a" request)]))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (cprobe name connector)
  (define (print-probe value)
    (printf "[P] ~a : ~a (~a)\n" name value (connector 'informant)))
  (define (process-new-value) (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond
      [(eq? request 'I-hava-a-value)
       (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [(eq? request 'type) 'probe]
      [else (error 'probe "Unknown request: ~a" request)]))
  (connect connector me)
  ((connector 'name) name)
  me)
------
(define (averager a b c)
  (let ([s (make-connector)] [w (make-connector)])
    (cprobe 's s)
    (cprobe 'w w)
    (adder a b s)
    (multiplier s w c)
    (constant 0.5 w)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)

(cprobe 'a a)
(cprobe 'b b)
(cprobe 'c c)

(set-value! a 18 'user)
(set-value! b 26 'user)
(?== 22.0 (get-value c))
(forget-value! b 'user)
(set-value! b 27 'user)
(?== 22.5 (get-value c))
(forget-value! a 'user)
(set-value! c 20 'user)
(?== 13.0 (get-value a))
)


;;; ex 3.34
(ex 34
(define (squarer a b) (multiplier a a b) 'ok)
;; 缺陷: multiplier 的两个a参数是独立的, 当b改变时无法正常工作

(define a (make-connector))
(define b (make-connector))
(squarer a b)

(cprobe 'a a)
(cprobe 'b b)

(set-value! a 9 'user)
(forget-value! a 'user)
(set-value! b 100 'user)
)


;;; ex 3.35
(ex 35
(define (squarer a b)
  (define (process-new-value)
    (cond
      [(has-value? b)
       (cond
         [(< (get-value b) 0) (error 'squarer "square less than 0: ~a" (get-value b))]
         [else (set-value! a (sqrt (get-value b)) me)])]
      [(has-value? a)
       (set-value! b (sqr (get-value a)) me)]))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-hava-a-value)
       (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [(eq? request 'type) 'probe]
      [else (error 'squarer "Unknown request: ~a" request)]))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))
(squarer a b)

(cprobe 'a a)
(cprobe 'b b)

(set-value! a 9 'user)
(?== 81 (get-value b))
(forget-value! a 'user)
(set-value! b 100 'user)
(?== 10 (get-value a))
)


;;; ex 3.36
(ex 36
;; (define a (make-connector))
;; (define b (make-connector))
;; (set-value! a 10 'user)
;;
;; [global: {a} {b} {set-value!} {inform-about-value}] `(set-value! a 10 'user)`
;; [E1 :->global {connector => a} {new-value 10} {informant => 'user}] `((a 'set-value!) 10 'user)`
;; [E1 :->global {connector => a} {new-value 10} {informant => 'user}] `(a 'set-value!)`
;; [E2 :->EX1 {request => 'set-value!}] `(cond ...)`
;; [E2 :->EX1 {request => 'set-value!}] `set-my-value`
;; [E1 :->global {connector => a} {new-value 10} {informant => 'user}] `(set-my-value 10 'user)`
;; [E3 :->EX2 {newval => 10} {setter => 'user}] `(cond ...)`
;; [EX2 :-> EX3 {value => 10} {informant => 'user} {constraints => '(...)}]
;; [E3 :->EX2 {newval => 10} {setter => 'user}] `(for-each-except 'user inform-about-value constraints)`
)


;;; ex 3.37
(ex 37
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ([z (make-connector)])
    (adder x y z)
    z))

(define (c- x y)
  (let ([z (make-connector)])
    (adder y z x)
    z))

(define (c* x y)
  (let ([z (make-connector)])
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ([z (make-connector)])
    (multiplier y z x)
    z))

(define (cv x)
  (let ([z (make-connector)])
    (constant x z)
    z))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(cprobe 'C C)
(cprobe 'F F)

(set-value! C 100 'user)
(?== 212 (get-value F))
(forget-value! C 'user)
(set-value! F 122 'user)
(?== 50 (get-value C))

((lambda ()
;; 一种适用性更好的表达方式
(define (cv x)
  (let ([z (make-connector)])
    (constant x z)
    z))

(define ((wrap cop) x)
  (cond
    [(number? x) (cop x)]
    [else x]))

(define (c+ a1 a2 . w)
  (define (helper c1 c2 . cs)
    (let ([z (make-connector)])
      (cond
        [(null? cs) (adder c1 c2 z)]
        [else (adder c1 (apply helper (cons c2 cs)) z)])
      z))
  (apply helper (map (wrap cv) (cons a1 (cons a2 w)))))

(define (c- a . w)
  (define (c0- x)
    (cond
      [(number? x) (cv (- x))]
      [else
       (let ([z (make-connector)])
         (adder x z (cv 0))
         z)]))
  (cond
    [(null? w) (c0- a)]
    [else (apply c+ (cons a (map c0- w)))]))

(define (c* a1 a2 . w)
  (define (helper c1 c2 . cs)
    (let ([z (make-connector)])
      (cond
        [(null? w) (multiplier c1 c2 z)]
        [else (multiplier c1 (apply c* (cons c2 cs)) z)])
      z))
  (apply helper (map (wrap cv) (cons a1 (cons a2 w)))))

(define (c/ a . w)
  (define (c1/ x)
    (cond
      [(number? x) (cv (/ 1 x))]
      [else
       (let ([z (make-connector)])
         (multiplier x z (cv 1))
         z)]))
  (cond
    [(null? w) (c1/ a)]
    [else (apply c* (cons a (map c1/ w)))]))

(define (c= a1 a2)
  (define (process-new-value)
    (cond
      [(has-value? a1)
       (set-value! a2 (get-value a1) me)]
      [(has-value? a2)
       (set-value! a1 (get-value a2) me)]))
  (define (process-forget-value)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'I-hava-a-value)
       (process-new-value)]
      [(eq? request 'I-lost-my-value)
       (process-forget-value)]
      [(eq? request 'type) 'c=]
      [else (error 'squarer "Unknown request: ~a" request)]))
  (connect a1 me)
  (connect a2 me)
  me)

(@>> "----------")
(define C (make-connector))
(define F (make-connector))
;; 9 * c = 5 * (F - 32)
(c= (c* 9 C)
    (c* 5 (c- F 32)))

(cprobe 'C C)
(cprobe 'F F)

(set-value! C 100 'user)
(?== 212 (get-value F))
(forget-value! C 'user)
(set-value! F 122 'user)
(?== 50 (get-value C))
(void)
))
)


;;; ex 3.38
(ex 38
;; 顺序运行:
;; - 45 peter -> paul -> mary
;; - 35 peter -> mary -> paul
;; - 45 paul -> peter -> mary
;; - 50 paul -> mary -> peter
;; - 40 mary -> peter -> paul
;; - 40 mary -> paul -> peter
((lambda ()
(define balance 100)
(define peter (lambda () (set! balance (+ balance 10))))
(define paul (lambda () (set! balance (- balance 20))))
(define mary (lambda () (set! balance (- balance (/ balance 2)))))

(define actions (make-hash))
(hash-set! actions 'peter (lambda () (set! balance (+ balance 10))))
(hash-set! actions 'paul (lambda () (set! balance (- balance 20))))
(hash-set! actions 'mary (lambda () (set! balance (- balance (/ balance 2)))))

(@>> "serial")
(for-each
  (lambda (serial)
    (set! balance 100)
    (for-each (lambda (k) ((hash-ref actions k))) serial)
    (printf "~a : ~a\n" balance (string-join (map symbol->string serial) " -> ")))
  (arrange (hash-keys actions)))
))

;; 交错进行:
;; peter    |   paul    |   mary
;; <- b     | <- b      | <- b
;; +10      | -20       | /2
;; -> b     | -> b      | <- b
;;          |           | -/2
;;          |           | -> b
((lambda ()
(define balance 100)

(define (make-serial . ss)
  (define (helper . ss)
    (cond
      [(or (null? ss) (null? (cdr ss))) ss]
      [(null? (cdr (cdr ss)))
       (let ([lst1 (car ss)] [lst2 (cadr ss)])
         (append
           (map (lambda (e) (cons (car lst1) e)) (apply make-serial (list (cdr lst1) lst2)))
           (map (lambda (e) (cons (car lst2) e)) (apply make-serial (list lst1 (cdr lst2))))))]
      [else
       (foldr
         (lambda (e rst)
           (append (apply make-serial (list (car ss) e)) rst))
         '()
         (apply make-serial (cdr ss)))]))
  (apply helper (filter (lambda (e) (not (null? e))) ss)))

(define (group instructs)
  (let loop ([ins (reverse instructs)] [init #t] [rst '()])
    (cond
      [(null? ins) rst]
      [else
       (let* ([head (car ins)]
              [new-group (memq (car head) '(loadb saveb))]
              [new-init (if init (not new-group) #f)])
         (cond
           [(null? rst)
            (loop (cdr ins) new-init (cons (list (car ins)) rst))]
           [(or init (not new-group))
            (loop (cdr ins) new-init (cons (cons (car ins) (car rst)) (cdr rst)))]
           [new-group
            (loop (cdr ins) new-init (cons (list (car ins)) rst))]))])))

(define peter-instruct-groups
  (group '((loadb ra) (add ra 10) (saveb ra))))

(define paul-instruct-groups
  (group '((loadb ra) (sub ra 20) (saveb ra))))

(define mary-instruct-groups
  (group '((loadb ra) (sar ra) (loadb rb) (sub rb ra) (saveb rb))))

(define (make-account name instruct-groups verbose)
  (define registers (make-hash '((ra . #f) (rb . #f))))
  (define (get-reg-value reg) (hash-ref registers reg))
  (define (set-reg-value! reg val) (hash-set! registers reg val))
  (define (get-reg/instant-value val) (if (symbol? val) (get-reg-value val) val))
  (define groups instruct-groups)
  (define (run-instruct instruct)
    (let* ([op (car instruct)] [reg (cadr instruct)])
      (cond
        [(eq? op 'loadb)
         (set-reg-value! reg balance)]
        [(eq? op 'saveb)
         (set! balance (get-reg-value reg))]
        [(eq? op 'add)
         (let ([reg-value (get-reg-value reg)] [num (get-reg/instant-value (caddr instruct))])
           (set-reg-value! reg (+ reg-value num)))]
        [(eq? op 'sub)
         (let ([reg-value (get-reg-value reg)] [num (get-reg/instant-value (caddr instruct))])
           (set-reg-value! reg (- reg-value num)))]
        [(eq? op 'sar)
         (let ([reg-value (get-reg-value reg)])
           (set-reg-value! reg (arithmetic-shift reg-value -1)))]))
    (when verbose (printf "[~a] balance: ~a, regs: ~a, ins: ~a\n" name balance registers instruct)))
  (define (dispatch m)
    (cond
      [(eq? m 'run)
       (unless (null? groups)
         (let ([first-group (car groups)])
           (for-each run-instruct first-group)
           (set! groups (cdr groups))))]))
  dispatch)

(define verbose #f)
(define (make-peter) (make-account "peter" peter-instruct-groups verbose))
(define (make-paul)  (make-account "paul " paul-instruct-groups verbose))
(define (make-mary)  (make-account "mary " mary-instruct-groups verbose))

(@>> "\ncross")

(define collections '())
(for-each
  (lambda (steps)
    (set! balance 100)
    (define accounts (make-hash (list (cons 'peter (make-peter))
                                      (cons 'paul (make-paul))
                                      (cons 'mary (make-mary)))))
    (for-each (lambda (step) ((hash-ref accounts step) 'run)) steps)
    (unless (memq balance collections) (set! collections (cons balance collections))))
  (make-serial
    (build-list (length peter-instruct-groups) (const 'peter))
    (build-list (length paul-instruct-groups) (const 'paul))
    (build-list (length mary-instruct-groups) (const 'mary))))

(printf "[ALL] ~a\n" collections)
))
)


;;; ex 3.39
(ex 39
;; P1: (set! x ((s (lambda () (* x x)))))
;; P2: (set! x (+ x 1))
;;
;; P1s: (readx) (read x) (* x x)
;; P1n: (writex)
;; P1: (P1s) (P1n)
;; P2s: (readx) (+ x 1) (writex)
;; P2: (P2s)
;;
;; 根据后文 maks-serializer 的 mutex 实现, P1s 和 P2s 之间不能交错, 但 P1 其他部分(P1n)可能在 P2s 中间执行 
;;
;; 121 : (P2s) (P1s) (P1n)
;; 101 : (P1s) (P1n) (P2s: readx) (P2s: + x 1) (P2x: writex)
;; 11  : (P1s) (P2s: readx) (P1n) (P2s: + x 1) (P2x: writex)
;; 11  : (P1s) (P2s: readx) (P2s: + x 1) (P1n) (P2x: writex)
;; 100 : (P1s) (P2s: readx) (P2s: + x 1) (P2x: writex) (P1n)
)


;;; ex 3.40
(ex 40
;; P1: (set! x (* x x))
;; P2: (set! x (* x x x))
;;
;; P1: (readx) (readx) (* x x) (writex)
;; P2: (readx) (readx) (readx) (* x x x) (writex)
;;
;; 1,000,000 : (P1) (P2) | (P2) (P1)
;; 10,000    : (P1:readx) (P2) (P1:readx) (P1:writex)
;; 100       : (P1:readx) (P1:readx) (P2) (P1:writex)
;; 100,000   : (P2:readx) (P1) (P2:readx) (P2:readx) (P2:writex)
;; 10,000    : (P2:readx) (P2:readx) (P1) (P2:readx) (P2:writex)
;; 1,000     : (P2:readx) (P2:readx) (P2:readx) (P1) (P2:writex)
;;
;; 串行化之后:
;; 1,000,000 : (P1) (P2) | (P2) (P1)
)


;;; ex 3.41
(ex 41
;; 不同意
;; 按书中的实现方式 (lambda () balance) 仅读取balance数值并返回给用户, 此过程无法被继续拆分
;; 不管获取时机与 withdraw 或 deposit 关系如何, 用户都能获取到当前时刻正确的 balance, 没有必要再进行串行化
;; 相反地, 进行串行化后用户获得的信息可能不符合其预期 (延迟)
;; 例如: 当有很多个账户同时获取 balance 时, 如果串行化则有的用户需要等待, 这是完全无必要的
)


;;; ex 3.42
(ex 42
;; 修改安全
;; 从功能上看, 两个版本是相同的
;; 从性能上看, 修改后的版本在大并发下所消耗的资源更少
)


;;; ex 3.43
(ex 43
;; 顺序执行的情况下, 10/20/30 进行整体交换, 且同一个账号不会被同时操作, 所以任意次并发交换后余额仍是 10/20/30
;;
;; (a1:10 | a2:20 | a3:30)
;; a1 <=> a2, a1 <=> a3
;; (a1:40 | a2:10 | a3:10) : (a1 deposit 10) (a1 deposit 20) (a2 withdraw 10) (a3 withdraw 20)
;; 
;; 同一账号内部进行了串行化, 每次账号的读和写操作都在一个串行队列内, 这样 withdraw 和 deposit 一定成对出现且不会交错进行
;; 所以3个账号的余额总和保持不变
;;
;; (a1:10 | a2:20 | a3:30)
;; a1 <=> a2, a1 <=> a3
;; (a1 deposit 10) (a1 deposit 20) (a2 withdraw 10) (a3 withdraw 20)
;; (a1 readb 10) (a1 readb 10) (a1 + 10 10) (a1 + 10 20) (a1 writeb 20) (a1 writeb 30) (a2 ...) (a3 ...)
;; -> (a1:30 | a2:10 | a3:10) -> total:50
)


;;; ex 3.44
(ex 44
;; Louis 是错误的
;; 转移问题和交换问题的本质区别在于:
;;     转移中的amount是独立的, 与账号本身没有任何关系, 产生转移关系的两个账号也相互独立
;;     交换问题中的金额是通过计算两个账号的余额差值获得的, 与账号本身有关, 产生转移关系的两个账号存在绑定关系
;; 所以对于转移问题而言, 只要withdraw和deposit成对出现且账号本身串行化即可满足需求, 不需要关注操作的顺序
;;
;; 以 ex 3.43 前一问为例
;; (a1:10 | a2:20 | a3:30)
;; a1 <=10= a2, a1 <=10= a3
;; (a1:40 | a2:10 | a3:10) : (a1 deposit 10) (a1 deposit 20) (a2 withdraw 10) (a3 withdraw 20)
;; a2 和 a3 分别向 a1 转移 10, 最终结果符合预期 (实际上, 不论后续操作的顺序如何结果都保持不变)
)


;;; ex 3.45
(ex 45
;; 考虑调用 serialized-exchange 时, withdraw/deposit 过程内部实际上被同一个 balance-serializer 串行化两次
;; 会导致程序无法运行
)


;;; ex 3.46
(ex 46
;; (if (test-and-set! cell) (the-mutex 'acquire))
;; (define (test-and-set! cell)
;;   (if (car cell) true (begin (set-car! cell true) false)))
;;
;; 假设有 (car cell) 为 #f, 有两个过程 p1, p2 同时获取 mutex
;; (p1 (car cell) -> #f) (p2 (car cell) -> #f) (p1 (set-car! cell true) false) (p2 (set-car! cell true) false)
)


;;; ex 3.47
(ex 47
(define (make-mutex n)
  (let ([cell (list n)])
    (define (the-mutex m)
      (cond
        [(eq? m 'acquire)
         (when (test-and-set! cell) (the-mutex 'acquire))]
        [(eq? m 'release)
         (clear! cell n)]))
    the-mutex))

(define (clear! cell n) (set-car! cell n))

(define (test-and-set! cell)
  (let ([cell-val (car cell)])
    (cond
      [(= 0 cell-val) #t]
      [else (set-car! cell (- cell-val 1)) #f])))
)


;;; ex 3.48
(ex 48
;; Peter: a1 <=> a2, Paul: a2 <=> a1
;; a1.code < a2.code
;; 执行顺序: (Peter/Paul.a1) (Peter/Paul.a2)
(define (exchange account1 account2)
  (let ([difference (- (account1 'balance) (account2 'balance))])
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ([flag1 (account1 'flag)] [flag2 (account2 'flag)]
        [serializer1 (account1 'serializer)] [serializer2 (account2 'serializer)])
    (cond
      [(< flag1 flag2)
       ((serializer1 (serializer2 exchange)) account1 account2)]
      [else
       ((serializer2 (serializer1 exchange)) account1 account2)])))

(define (make-account-and-serializer balance flag)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ([balance-serializer (make-serializer)])
    (define (dispatch m)
      (cond
        [(eq? m 'withdraw) withdraw]
        [(eq? m 'deposit) deposit]
        [(eq? m 'balance) balance]
        [(eq? m 'serializer) balance-serializer]
        [(eq? m 'flag) flag]))
    dispatch))

;; make-serializer 定义略
(define (make-serializer) #f)
)


;;; ex 3.49
(ex 49
;; 参考 ex 3.48
;; 当 a1 和 a2 的访问顺序有外部约束时, 则进程的标识编号失效
;; 如 Peter 顺序必须 a1 -> a2
;;    Paul  顺序必须 a2 -> a1
)


;;; ex 3.50
(ex 50
(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
        (apply proc (map stream-first argstreams))
        (apply stream-map (cons proc (map stream-rest argstreams))))))

(define s1 (stream 2 3))
(define s2 (stream 4 5))
(define s3 (stream-map * s1 s2))
(?== 8  (stream-ref s3 0))
(?== 15 (stream-ref s3 1))
)


;;; ex 3.51
(ex 51
;; 输出结果与具体实现方式有关
(define empty-stream/s '())

(define (stream-empty?/s s) (null? s))

(define (force/s x) (x))

(define (stream-cdr/s s) (force/s (cdr s)))

((lambda ()
;; 书中的实现方式
;; (stream-cons a b) => (cons a (delay b))
(define (stream-car/sicp s) (car s))

(define (stream-ref/s s n)
  (if (= n 0)
      (stream-car/sicp s)
      (stream-ref/s (stream-cdr/s s) (- n 1))))

(define (stream-map/s proc . argstreams)
  (if (stream-empty?/s (car argstreams))
      empty-stream/s
      (stream-cons/sicp
        (apply proc (map stream-car/sicp argstreams))
        (apply stream-map/s (cons proc (map stream-cdr/s argstreams))))))

(define (stream-enumerate-interval/s low high)
  (cond
    [(> low high) empty-stream]
    [else (stream-cons/sicp low (stream-enumerate-interval/s (+ 1 low) high))]))
(define (show x) (printf "[~a]\n" x) x)

(@>> "------ sicp ------")
(define s1 (stream-enumerate-interval/s 0 10))
(@>> "--- 1 ---")
(define x (stream-map/s show s1))
;; output: 0
(@>> "--- 2 ---")
(?== 5 (stream-ref/s x 5))
;; output: (1 2 3 4 5)
(@>> "--- 3 ---")
(?== 7 (stream-ref/s x 7))
;; output: (6 7)
))

((lambda ()
;; racket 实现方式
;; (stream-cons a b) => (cons (delay a) (delay b))
(define (stream-car/racket s) (force/s (car s)))

(define (stream-ref/s s n)
  (if (= n 0)
      (stream-car/racket s)
      (stream-ref/s (stream-cdr/s s) (- n 1))))

(define (stream-map/s proc . argstreams)
  (if (stream-empty?/s (car argstreams))
      empty-stream/s
      (stream-cons/racket
        (apply proc (map stream-car/racket argstreams))
        (apply stream-map/s (cons proc (map stream-cdr/s argstreams))))))

(define (stream-enumerate-interval/s low high)
  (cond
    [(> low high) empty-stream]
    [else (stream-cons/racket low (stream-enumerate-interval/s (+ 1 low) high))]))
(define (show x) (printf "[~a]\n" x) x)

(@>> "------ racket ------")
(define s1 (stream-enumerate-interval/s 0 10))
(@>> "--- 1 ---")
(define x (stream-map/s show s1))
;; output: void
(@>> "--- 2 ---")
(?== 5 (stream-ref/s x 5))
;; output: 5
(@>> "--- 3 ---")
(?== 7 (stream-ref/s x 7))
;; output: 7
))
)


;;; ex 3.52
(ex 52
;; 按书中的实现方式
(define (force/s x) (x))

(define empty-stream/s '())

(define (stream-empty?/s s) (null? s))

(define (stream-car/s s) (car s))

(define (stream-cdr/s s) (force/s (cdr s)))

(define (stream-ref/s s n)
  (cond
    [(= n 0) (stream-car/s s)]
    [else (stream-ref/s (stream-cdr/s s) (- n 1))]))

(define (display-stream/s s)
  (cond
    [(stream-empty?/s s) (printf "\n")]
    [else
     (printf "~a " (stream-car/s s))
     (display-stream/s (stream-cdr/s s))]))

((lambda ()
(@>> "------ MEMO ------")

(@alias stream-cons/s stream-cons/sicp)

(define (stream-map/s proc . streams)
  (cond
    [(stream-empty?/s (car streams)) empty-stream/s]
    [else
     (stream-cons/s
       (apply proc (map stream-car/s streams))
       (apply stream-map/s (cons proc (map stream-cdr/s streams))))]))

(define (stream-filter/s pred? s)
  (cond
    [(stream-empty?/s s) empty-stream/s]
    [(pred? (stream-car/s s))
     (stream-cons/s (stream-car/s s) (stream-filter/s pred? (stream-cdr/s s)))]
    [else (stream-filter/s pred? (stream-cdr/s s))]))

(define (stream-enumerate-interval/s low high)
  (cond
    [(> low high) empty-stream/s]
    [else (stream-cons/s low (stream-enumerate-interval/s (+ low 1) high))]))

(define sum 0)
(?== 0 sum)

(define (accum x)
  (set! sum (+ x sum))
  sum)
(?== 0 sum)

(define seq (stream-map/s accum (stream-enumerate-interval/s 1 20)))
;; (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;; [x=1 => sum=1] seq: (1 ...)
(?== 1 sum)

(define y (stream-filter/s even? seq))
;; [x=2 => sum=3] seq: (1 3 ...)
;; [x=3 => sum=6] seq: (1 3 6 ...) y: (6 ...)
(?== 6 sum)

(define z (stream-filter/s (lambda (x) (= (remainder x 5) 0)) seq))
;; [x=4 => sum=10] seq: (1 3 6 10 ...) z: (10 ...)
(?== 10 sum)

(?== 136 (stream-ref/s y 7))
;; (6 10 28 36 66 78 120 136 ...)
(?== 136 sum)

(display-stream/s z)
;; (10 15 45 55 105 120 190 210)
(?== 210 sum)
))

((lambda ()
(@>> "------ NO-MEMO ------")

(@alias stream-cons/s stream-cons/sicp/nm)

(define (stream-map/s proc . streams)
  (cond
    [(stream-empty?/s (car streams)) empty-stream/s]
    [else
     (stream-cons/s
       (apply proc (map stream-car/s streams))
       (apply stream-map/s (cons proc (map stream-cdr/s streams))))]))

(define (stream-filter/s pred? s)
  (cond
    [(stream-empty?/s s) empty-stream/s]
    [(pred? (stream-car/s s))
     (stream-cons/s (stream-car/s s) (stream-filter/s pred? (stream-cdr/s s)))]
    [else (stream-filter/s pred? (stream-cdr/s s))]))

(define (stream-enumerate-interval/s low high)
  (cond
    [(> low high) empty-stream/s]
    [else (stream-cons/s low (stream-enumerate-interval/s (+ low 1) high))]))

(define sum 0)
(?== 0 sum)

(define (accum x)
  (set! sum (+ x sum))
  sum)
(?== 0 sum)

(define seq (stream-map/s accum (stream-enumerate-interval/s 1 20)))
;; [x=1 => sum=1] seq: (1 ...)
(?== 1 sum)

(define y (stream-filter/s even? seq))
;; [x=2 => sum=3] seq: (1 3 ...)
;; [x=3 => sum=6] seq: (1 3 6 ...) y: (6 ...)
(?== 6 sum)

(define z (stream-filter/s (lambda (x) (= (remainder x 5) 0)) seq))
;; [x=2 => sum=8] seq: (1 8 ...)
;; [x=3 => sum=11] seq: (1 8 11 ...)
;; [x=4 => sum=15] seq: (1 8 15 ...), z: (15 ...)
(?== 15 sum)

(?== 162 (stream-ref/s y 7))
;; 0 => seq: (1 3 6 ...), y: (6 ...)
;; 1 => [x=4 => sum=19] y: (6 ...)
;; 1 => [x=5 => sum=24] y: (6 24 ...)
;; 2 => [x=6 => sum=30] y: (6 24 30 ...)
;; 3 => [x=9 => sum=54] y: (6 24 30 54 ...)
;; 4 => [x=10 => sum=64] y: (6 24 30 54 64 ...)
;; 5 => [x=13 => sum=100] y: (6 24 30 54 64 100 ...)
;; 6 => [x=14 => sum=114] y: (6 24 30 54 64 100 114 ...)
;; 7 => [x=17 => sum=162] y: (6 24 30 54 64 100 114 162 ...)
(?== 162 sum)

(display-stream/s z)
;; seq: (1 8 15 ...), z: (15 ...)
;; [x=5 => sum=167]
;; [x=6 => sum=173]
;; [x=7 => sum=180] z: (15 180 ...)
;; [x=12 => sum=230] z: (15 180 230 ...)
;; [x=17 => sum=305] z: (15 180 230 305)
;; [x=20 => sum=362]
(?== 362 sum)
))
)


;;; ex 3.53
(ex 53
(define (stream-take-list s n) (stream->list (stream-take s n)))

(define (stream-map f . ss)
  (cond
    [(stream-empty? (car ss)) empty-stream]
    [else
     (stream-cons
       (apply f (map stream-first ss))
       (apply stream-map (cons f (map stream-rest ss))))]))

(define (add-streams s1 s2) (stream-map + s1 s2))
------
(define s (stream-cons 1 (add-streams s s)))
;; (1 2 4 8 16 ...)

(?== '(1 2 4 8 16 32) (stream-take-list s 6))
)


;;; ex 3.54
(ex 54
(define ones (stream-cons 1 ones))

(define integers (stream-cons 1 (add-streams integers ones)))

(define (mul-streams s1 s2) (stream-map * s1 s2))
------
(define factorials (stream-cons 1 (mul-streams integers factorials)))
(?== '(1 1 2 6 24 120 720) (stream-take-list factorials 7))
)


;;; ex 3.55
(ex 55
(define (partial-sums s)
  (define sums (stream-cons (stream-first s) (add-streams sums (stream-rest s))))
  sums)
------
(define (partial-sums/v2 s)
  (define sums (add-streams s (stream-cons 0 sums)))
  sums)

(?== '(1 3 6 10 15 21) (stream-take-list (partial-sums integers) 6))
(?== '(1 3 6 10 15 21) (stream-take-list (partial-sums/v2 integers) 6))
)


;;; ex 3.56
(ex 56
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))
------
(define (merge s1 s2)
  (cond
    [(stream-empty? s1) s2]
    [(stream-empty? s2) s1]
    [else
     (let ([s1car (stream-first s1)] [s2car (stream-first s2)])
       (cond
         [(< s1car s2car)
          (stream-cons s1car (merge (stream-rest s1) s2))]
         [(> s1car s2car)
          (stream-cons s2car (merge s1 (stream-rest s2)))]
         [else
          (stream-cons s1car (merge (stream-rest s1) (stream-rest s2)))]))]))

(define S (stream-cons 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(?== '(1 2 3 4 5 6 8 9 10 12 15 16 18 20) (stream-take-list S 14))
)


;;; ex 3.57
(ex 57
(define fibs (stream-cons 1 (stream-cons 1 (add-streams fibs (stream-rest fibs)))))
;; memo: n => n => O(n)
;; no-memo: n => fib(n) => O(c^n)
)


;;; ex 3.58
(ex 58
(define (expand num den radix)
  (stream-cons
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
;; 分数 num/den 转小数 0.s
(?== '(1 4 2 8 5 7 1 4) (stream-take-list (expand 1 7 10) 8))
(?== '(3 7 5 0 0 0) (stream-take-list (expand 3 8 10) 6))
)


;;; ex 3.59
(ex 59
(define (integrate-series s) (stream-map / s integers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series (stream-cons 0 (integrate-series cosine-series)))
------
(?== '(1 1 1/2 1/6 1/24 1/120) (stream-take-list exp-series 6))
(?== '(0 1 0 -1/6 0 1/120) (stream-take-list sine-series 6))
(?== '(1 0 -1/2 0 1/24 0) (stream-take-list cosine-series 6))
)


;;; ex 3.60
(ex 60
(define add-series add-streams)

(define (mul-series s1 s2)
  (stream-cons
    (* (stream-first s1) (stream-first s2))
    (add-streams
      (add-streams
        (scale-stream (stream-rest s1) (stream-first s2))
        (scale-stream (stream-rest s2) (stream-first s1)))
      (stream-cons
        0
        (mul-series (stream-rest s1) (stream-rest s2))))))
------
(define s1 (add-series (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))
(?== '(1 0 0 0 0 0) (stream-take-list s1 6))
)


;;; ex 3.61
(ex 61
(define (reciprocal-series s)
  (define x
    (stream-cons 1
      (scale-stream (mul-series (stream-rest s) x) -1)))
  x)
------
(?== '(1 -1 1/2 -1/6 1/24 -1/120) (stream-take-list (reciprocal-series exp-series) 6))
)


;;; ex 3.62
(ex 62
(define (div-series s1 s2)
  (let ([c2 (stream-first s2)])
    (cond
      [(= 0 c2)
       (error 'div-series "Divided by series in which constant term is 0: ~a" (stream-take-list s2 3))]
      [else
       (mul-series
         (scale-stream s1 (/ 1 c2))
         (reciprocal-series (scale-stream s2 (/ 1 c2))))])))

(define tan-series (div-series sine-series cosine-series))
(?== '(0 1 0 1/3 0 2/15) (stream-take-list tan-series 6))
)


;;; ex 3.63
(ex 63
;; 方法中存在对 sqrt-stream 的递归调用, 每次调用时都会生成新的流, 即delay方法的memo不起作用
;; 如果不使用memo, 则两个版本效率相同
)


;;; ex 3.64
(ex 64
(define (sqrt-stream x)
  (define (sqrt-improve guess x) (average guess (/ x guess)))
  (define guesses
    (stream-cons
      1.0
      (stream-map
        (lambda (guess) (sqrt-improve guess x))
        guesses)))
  guesses)

(define (stream-limit s tolerance)
  (cond
    [(stream-empty? s) #f]
    [(stream-empty? (stream-rest s)) #f]
    [else
     (let ([e1 (stream-first s)] [e2 (stream-first (stream-rest s))])
       (if (< (abs (- e1 e2)) tolerance)
           e2
           (stream-limit (stream-rest s) tolerance)))]))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(?~= 2.0 (sqr (sqrt 2.0 1e-6)))
)


;;; ex 3.65
(ex 65
(define (summands n)
  (stream-cons
    (/ 1.0 n)
    (stream-map - (summands (+ n 1)))))

(define s (partial-sums (summands 1)))
(?~= 2.0 (exp (stream-ref s 100)) 0.01)
(?~= 2.0 (exp (stream-ref s 1000)) 0.001)
;; 收敛很慢
)


;;; ex 3.66
(ex 66
(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
        (stream-first s1)
        (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-first s) (stream-first t))
    (interleave
      (stream-map
        (lambda (x) (list (stream-first s) x))
        (stream-rest t))
      (pairs (stream-rest s) (stream-rest t)))))
------
(define (pos i j)
  (let loop ([j j] [rst (- (expt 2 i) 2)])
    (cond
      [(= j i) rst]
      [(= j (+ i 1)) (loop (- j 1) (+ rst (expt 2 (- i 1))))]
      [else (loop (- j 1) (+ rst (expt 2 i)))])))
;; if j == i   : 2^i - 1
;; if j == i+1 : 2^i - 1 + 2^(i-1)
;; if j >  i+1 : 2^i - 1 + 2^(i-1) + (j-i-1)*(2^i) => (j-i)*(2^i) + 2^(i-1) - 1

(define s (pairs integers integers))

(?== '(1 100) (stream-ref s 197))
(?== '(10 15) (stream-ref s (pos 10 15)))
)


;;; ex 3.67
(ex 67
(define (full-pairs s t)
  (let ([sf (stream-first s)] [sr (stream-rest s)]
        [tf (stream-first t)] [tr (stream-rest t)])
    (stream-cons
      (list sf tf)
      (interleave
        (stream-map (lambda (x) (list sf x)) tr)
        (interleave
          (stream-map (lambda (x) (list x tf)) sr)
          (full-pairs sr tr))))))

(define s (full-pairs integers integers))
(@>> (stream-take-list s 50))
)


;;; ex 3.68
(ex 68
;; 没有第一个元素, 程序一直等待, 无法继续运行
)


;;; ex 3.69
(ex 69
(define (triples s t u)
  (let ([s0 (stream-first s)] [sr (stream-rest s)]
        [t0 (stream-first t)] [tr (stream-rest t)]
        [u0 (stream-first u)] [ur (stream-rest u)])
    (stream-cons
      (list s0 t0 u0)
      (interleave
        (stream-map (lambda (x) (list s0 t0 x)) ur)
        (interleave
          (stream-map (lambda (x) (cons s0 x)) (pairs tr ur))
          (triples sr tr ur))))))

(define s (triples integers integers integers))
(define b
  (stream-filter
    (lambda (x)
      (let ([i (car x)] [j (cadr x)] [k (caddr x)])
        (= (* k k) (+ (* i i) (* j j)))))
    s))

(?== '((3 4 5) (6 8 10) (5 12 13)) (stream-take-list b 3))
)


;;; ex 3.70
(ex 70
(define (merge-weighted s t weight)
  (cond
    [(stream-empty? s) t]
    [(stream-empty? t) s]
    [else
     (let* ([s0 (stream-first s)] [t0 (stream-first t)]
            [s0w (weight s0)] [t0w (weight t0)])
       (cond
         [(< s0w t0w) (stream-cons s0 (merge-weighted (stream-rest s) t weight))]
         [else (stream-cons t0 (merge-weighted s (stream-rest t) weight))]))]))

(define (weighted-pairs s t weight)
  (let ([s0 (stream-first s)] [t0 (stream-first t)]
        [sr (stream-rest s)] [tr (stream-rest t)])
    (stream-cons
      (list s0 t0)
      (merge-weighted
        (stream-map (lambda (x) (list s0 x)) tr)
        (weighted-pairs sr tr weight)
        weight))))
------
(define s1 (weighted-pairs integers integers (lambda (p) (+ (car p) (cadr p)))))
(?== '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4)) (stream-take-list s1 6))

(define s2
  (stream-filter
    (lambda (x)
      (or (= 0 (remainder x 2))
          (= 0 (remainder x 3))
          (= 0 (remainder x 5))))
    integers))

(define s3 (weighted-pairs s2 s2 (lambda (p) (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))))
(?== '((2 2) (2 3) (2 4) (3 3) (2 5) (3 4)) (stream-take-list s3 6))
)


;;; ex 3.71
(ex 71
(define (weight p)
  (let ([i (car p)] [j (cadr p)])
    (+ (* i i i) (* j j j))))

(define (make-ramanujans)
  (define s1 (weighted-pairs integers integers weight))
  (define s2 (stream-map weight s1))
  (stream-map
    car
    (stream-filter
      (lambda (p) (= (car p) (cadr p)))
      (stream-map list s2 (stream-rest s2)))))

(define s (make-ramanujans))
(?== '(1729 4104 13832 20683 32832 39312) (stream-take-list s 6))
)


;;; ex 3.72
(ex 72
(define (weight p)
  (let ([i (car p)] [j (cadr p)])
    (+ (* i i) (* j j))))

(define s1 (weighted-pairs integers integers weight))

(define s2 (stream-map list s1 (stream-rest s1) (stream-rest (stream-rest s1))))

(define s3
  (stream-filter
    (lambda (p)
      (let ([p0 (car p)] [p1 (cadr p)] [p2 (caddr p)])
        (= (weight p0) (weight p1) (weight p2))))
    s2))

(define (print s n)
  (let ([x (stream-ref s n)])
    (printf "[~a] ~a\n" (weight (car x)) x)))

(print s3 0)
(print s3 1)
(print s3 2)
)


;;; ex 3.73
(ex 73
(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
      (add-streams
        (scale-stream integrand dt)
        int)))
  int)

(define ((RC r c dt) i v0)
  (add-streams
    (scale-stream i r)
    (integral (scale-stream i (/ 1.0 c)) v0 dt)))

(define RC1 (RC 5 1 0.5))

(define i (stream-map (lambda (ti) (sin (* pi ti 0.01))) integers))

(define v (RC1 i 0))
)


;;; ex 3.74
(ex 74
(define (sign-change-detector cv lv)
  (cond
    [(and (< lv 0) (>= cv 0)) 1]
    [(and (>= lv 0) (< cv 0)) -1]
    [else 0]))
------
(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 0 0 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cons 0 sense-data)))

(?== '(0 0 0 0 0 -1 0 0 0 0 1 0) (stream-take-list zero-crossings 12))
)


;;; ex 3.75
(ex 75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ([avpt (/ (+ (stream-first input-stream) last-value) 2)])
    (stream-cons
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings (stream-rest input-stream) (stream-first input-stream) avpt))))
)


;;; ex 3.76
(ex 76
(define (smooth s)
  (stream-map average s (stream-rest s)))

(define (make-zero-crossings input-stream last-value smooth)
  (let ([smoothed-stream (smooth input-stream)])
    (stream-map
      sign-change-detector
      smoothed-stream
      (stream-cons last-value smoothed-stream))))
)


;;; ex 3.77
(ex 77
(define (integral delayed-integrand initial-value dt)
  (stream-cons
    initial-value
    (let ([integrand (force delayed-integrand)])
      (if (stream-empty? integrand)
          empty-stream
          (integral
            (delay (stream-rest integrand))
            (+ (* dt (stream-first integrand)) initial-value)
            dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(@>> (stream-ref (solve (lambda (y) y) 1 0.001) 1000))
)


;;; ex 3.78
(ex 78
(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons
      initial-value
      (let ([integrand (force delayed-integrand)])
        (add-streams (scale-stream integrand dt) int))))
  int)
------
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (add-streams
      (scale-stream dy a)
      (scale-stream y b)))
  y)

(?~= 2.7181459 (stream-ref (solve-2nd 1 0 0.0001 1 1) 10000))
)


;;; ex 3.79
(ex 79
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(?~= 2.7181459 (stream-ref (solve-2nd (lambda (dy y) dy) 1 1 0.0001) 10000))
)


;;; ex 3.80
(ex 80
(define ((RLC R L C dt) vc0 il0)
  (define vc (integral (delay dvc) vc0 dt))
  (define il (integral (delay dil) il0 dt))
  (define dvc (scale-stream il (/ -1.0 C)))
  (define dil
    (add-streams
      (scale-stream vc (/ 1.0 L))
      (scale-stream il (/ (- R) L))))
  (cons vc il))

(define RLC1 (RLC 1 1 0.2 0.1))
(define pair (RLC1 10 0))

(@>> (stream-take-list (car pair) 10))
(@>> (stream-take-list (cdr pair) 10))
)


;;; ex 3.81
(ex 81
(define (rand-update x) (modulo (+ (* 17 x) 43) 100))

(define (rand-generate ops initial)
  (define rands
    (stream-cons
      initial
      (stream-map
        (lambda (op n)
          (let ([m (car op)])
            (cond
              [(eq? m 'g) (rand-update n)]
              [(eq? m 'r) (cadr op)])))
        ops
        rands)))
  rands)

(define ops (stream '(g) '(g) '(g) '(r 1) '(g) '(r 2) '(g) '(g)))
(?== '(0 43 74 1 1 60 2 77 52) (stream-take-list (rand-generate ops 0) 9))
)


;;; ex 3.82
(ex 82
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
      (/ passed (+ passed failed))
      (monte-carlo (stream-rest experiment-stream) passed failed)))
  (if (stream-first experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral pred? x1 x2 y1 y2)
  (define MAX 1000)
  (define (coord n low high)
    (+ low (* n (- high low) (/ 1.0 MAX))))
  (define (make-random-pairs)
    (stream-cons
      (cons (random MAX) (random MAX))
      (make-random-pairs)))
  (monte-carlo
    (stream-map
      (lambda (p)
        (let ([x (coord (car p) x1 x2)] [y (coord (cdr p) y1 y2)])
          (pred? x y)))
      (make-random-pairs))
    0 0))

(define (in-cycle x y) (<= (+ (* x x) (* y y)) 1))

(define pi (scale-stream (estimate-integral in-cycle -1 1 -1 1) 4.0))

(?~= 3.14 (stream-ref pi 100000) 0.01)
)


(run-ex 1 ~ 82)