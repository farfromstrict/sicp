#lang sicp
(#%require (all-except racket
  list cons car cdr pair? append))
(#%require sicp-pict)
(#%require (only r5rs
  list cons car cdr append pair?
  set-car! set-cdr!))

(#%require "common.rkt")


;;; 3.1
((lambda ()
(define (make-accumulator init)
  (let ([value init])
    (lambda (increment)
      (set! value (+ value increment))
      value)))

(define A (make-accumulator 5))

(mlog "ex-3.1"
(A 10)  ; => 15
(A 10)  ; => 25
)
))


;;; 3.2
((lambda ()
(define (make-monitored f)
  (let ([call-times 0])
    (define (dispatch msg)
      (cond
        [(eqv? msg 'how-many-calls?) call-times]
        [else
          (set! call-times (+ call-times 1))
          (f msg)]))
    dispatch))

(define s (make-monitored sqrt))

(mlog "ex-3.2"
(s 100)  ; => 10
(s 'how-many-calls?)  ; => 1
(s 225)  ; => 15
(s 'how-many-calls?)  ; => 2
)
))


;;; 3.3
((lambda ()
(define (make-account init-balance init-password)
  (let ([balance init-balance])
    (lambda (password action)
      (lambda (amount)
        (cond
          [(not (eqv? password init-password)) "Incorrect password"]
          [(eqv? action 'withdraw)
             (set! balance (- balance amount))
             balance]
          [(eqv? action 'deposit)
             (set! balance (+ balance amount))
             balance]
          [else (error 'make-account "unknown action" action)])))))

(define acc (make-account 100 'secret-password))

(mlog "ex-3.3"
((acc 'secret-password 'withdraw) 40)  ; => 60
((acc 'some-other-password 'deposit) 50)  ; => "Incorrect password"
)
))


;;; 3.4
((lambda ()
(define (call-the-cops) "Call 911")

(define (make-account init-balance init-password)
  (let ([balance init-balance] [incorrect-times 0])
    (lambda (password action)
      (lambda (amount)
        (if (eqv? password init-password)
          (begin
            (set! incorrect-times 0)
            (cond
              [(eqv? action 'withdraw)
                 (set! balance (- balance amount))
                 balance]
              [(eqv? action 'deposit)
                 (set! balance (+ balance amount))
                 balance]
              [else (error 'make-account "unknown action" action)]))
          (begin
            (set! incorrect-times (+ incorrect-times 1))
            (if (>= incorrect-times 7)
              (call-the-cops)
              "Incorrect password")))))))

(define password 'secret)
(define wrong-password 'wrong)

(define acc (make-account 100 password))

(mlog "ex-3.4"
((acc wrong-password 'withdraw) 50)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 50)  ; => "Incorrect password"
((acc password 'withdraw) 50)  ; => 50
((acc wrong-password 'withdraw) 30)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 30)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 30)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 30)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 30)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 30)  ; => "Incorrect password"
((acc wrong-password 'withdraw) 30)  ; => "Call 911"
((acc wrong-password 'withdraw) 30)  ; => "Call 911"
)
))


;;; 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      [(= trials-remaining 0) (exact->inexact (/ trials-passed trials))]
      [(experiment) (iter (dec trials-remaining) (inc trials-passed))]
      [else (iter (dec trials-remaining) trials-passed)]))
  (iter trials 0))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (* (monte-carlo trials pred)
     (- x2 x1)
     (- y2 y1)))

(define (in-circle-test origin-x origin-y radius)
  (define delta-count 1000)
  (let* ([left (- origin-x radius)]
         [right (+ origin-x radius)]
         [bottom (- origin-y radius)]
         [top (+ origin-y radius)]
         [delta (exact->inexact (/ (* 2 radius) delta-count))]
         [randx (random 0 (+ delta-count 1))]
         [randy (random 0 (+ delta-count 1))]
         [px (+ left (* delta randx))]
         [py (+ bottom (* delta randy))])
    (<= (+ (sqr (- px origin-x)) (sqr (- py origin-y)))
        (sqr radius))))

(define (estimate-pi trials)
  (estimate-integral
    (lambda ()
      (in-circle-test 0 0 1))
    -1 1 -1 1 trials))

((lambda ()
(mlog "ex-3.5"
(estimate-pi 100)
(estimate-pi 10000)
(estimate-pi 1000000)
)
))


;;; 3.6
((lambda ()
(define (random-update x) (inc x))

(define (make-random random-init)
  (let ([x random-init])
    (lambda (msg)
      (cond
        [(eqv? msg 'generate)
           (set! x (random-update x))
           x]
        [(eqv? msg 'reset)
           (lambda (new-value)
             (set! x new-value)
             x)]))))

(define rand (make-random 1))

(mlog "ex-3.6"
(rand 'generate)
(rand 'generate)
((rand 'reset) 100)
(rand 'generate)
(rand 'generate)
((rand 'reset) 100)
(rand 'generate)
(rand 'generate)
)
))


;;; 3.7
((lambda ()
(define (make-account init-balance init-password)
  (let ([balance init-balance])
    (lambda (password action)
      (lambda (amount)
        (cond
          [(not (eqv? password init-password)) "Incorrect password"]
          [(eqv? action 'withdraw)
             (set! balance (- balance amount))
             balance]
          [(eqv? action 'deposit)
             (set! balance (+ balance amount))
             balance])))))

(define (make-joint shared-account shared-account-password new-account-password)
  (lambda (password action)
    (lambda (amount)
      (cond
        [(not (eqv? password new-account-password)) "Incorrect password"]
        [else
          ((shared-account shared-account-password action) amount)]))))

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(mlog "ex-3.7"
((paul-acc 'no-secret 'withdraw) 20)  ; => "Incorrect password"
((paul-acc 'open-sesame 'withdraw) 20)  ; => "Incorrect password"
((paul-acc 'rosebud 'withdraw) 20)  ; => 80
((peter-acc 'rosebud 'withdraw) 20)  ; => "Incorrect password"
((peter-acc 'open-sesame 'withdraw) 20)  ; => 60
((paul-acc 'rosebud 'withdraw) 20)  ; => 40
)
))


;;; 3.8
((lambda ()
(define x 0)

(define (f n)
  (let ([y x])
    (set! x (+ x n))
    y))

(hlog "ex-3.8")
(set! x 0)
(printf "~a\n" (+ (f 0) (f 1)))  ; => 0
(set! x 0)
(printf "~a\n" (+ (f 1) (f 0)))  ; => 1
(tlog)
))


;;; 3.9
; ignored


;;; 3.10
((lambda ()
(define make-withdraw
  (lambda (init-amount)
    ((lambda (balance)
       (lambda (amount)
         (if (>= balance amount)
             (begin
               (set! balance (- balance amount))
               balance)
             "Insufficient funds")))
       init-amount)))

(hlog "ex-3.10")
(define W1 (make-withdraw 100))
(printf "~a\n" (W1 50))
(define W2 (make-withdraw 100))
(printf "~a\n" (W2 60))
(tlog)

; [Frame] GLOBAL
; [ Code] (define make-withdraw (lambda (init-amount) ...))
; [ Eval] (lambda (init-amount) ...) (env: GLOBAL)
; [ Proc] <proc#1> => params: init-amount, body: ..., env -> GLOBAL
; [ Eval] (define make-withdraw <proc#1>) (env: GLOBAL)
; [Frame] GLOBAL <= make-withdraw: <proc#1>
;
; [ Code] (define W1 (make-withdraw 100))
; [ Eval] (make-withdraw 100) (env: GLOBAL)
; [Apply] <proc#1> @ 100
; [Frame] E11 => init-amount: 100, env -> GLOBAL (env of <proc#1>)
; [ Eval] <proc#1> => ((lambda (balance) ...) init-amount) (env: E11)
; [ Eval] (lambda (balance) ...) (env: E11)
; [ Proc] <proc#2> => params: balance, body: ..., env -> E11
; [ Eval] ((lambda (balance) ...) init-amount) (env: E11)
; [Apply] <proc#2> @ 100
; [Frame] E12 => balance: 100, env -> E11 (env of <proc#2>)
; [ Eval] (lambda (amount) ...) (env: E12)
; [Frame] <proc#3> => params: amount, body: ..., env -> E12
; [ Eval] (define W1 <proc#3>) (env: GLOBAL)
; [Frame] GLOBAL <= W1: <proc#3>
;
; [ Code] (W1 50)
; [ Eval] (W1 50) (env: GLOBAL)
; [Apply] <proc#3> @ 50
; [Frame] E13 => amount: 50, env -> E12 (env of <proc#3>)
; [ Eval] (if ... balance amount ...) (env: E13)
; [ Eval] balance -> 100 (env: E12), amount -> 50 (env: E13)
; [ Eval] ...
; [  Rst] balance <- 50 (env: E12), <- 50
;
; [ Code] (define W2 (make-withdraw 100))
; [Frame] E21 => init-amount: 100, env -> GLOBAL
; [Frame] E22 => balance: 100, env -> E21
; [Frame] GLOBAL <= W2 -> <proc#3>
;
; [ Code] (W2 60)
; [Frame] E23 => amount: 60, env -> E22
; [  Rst] balance <- 40 (env: E22), <- 40
))


;;; 3.11
((lambda ()
(define make-account
  (lambda (balance)
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds")))
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))
        balance))
    (define dispatch
      (lambda (m)
        (cond
          [(eqv? m 'withdraw) withdraw]
          [(eqv? m 'deposit) deposit]
          [else (error 'make-account "unknown request" m)])))
    dispatch))

(define acc1 (make-account 50))
(define acc2 (make-account 100))

(mlog "ex-3.11"
((acc1 'deposit) 40)  ; => 90
((acc1 'withdraw) 60)  ; => 30
((acc2 'deposit) 40)  ; => 140
((acc2 'withdraw) 60)  ; => 80
)

; [Frame] GLOBAL
; [ Code] (define make-account (lambda (balance) ...))
; [ Eval] (lambda (balance) ...) (env: GLOBAL)
; [ Proc] <proc#1> => params: balance, body: ..., env -> GLOBAL
; [Frame] GLOBAL <= make-account: <proc#1>
;
; [ Code] (define acc1 (make-account 50))
; [ Eval] (make-account 50) (env: GLOBAL)
; [Apply] <proc#1> @ 50
; [Frame] E11 => balance: 50, env -> GLOBAL
; [ Eval] (define withdraw ...)... dispatch (env: E11)
; [ Proc] <proc#2> => params: amount, body: ..., env -> E11
; [ Proc] <proc#3> => params: amount, body: ..., env -> E11
; [ Proc] <proc#4> => params: m, body: ..., env -> E11
; [Frame] E11 <= withdraw: <proc#2>
; [Frame] E11 <= deposit: <proc#3>
; [Frame] E11 <= dispatch: <proc#4>
; [Frame] GLOBAL <= acc1: <proc#4>
;
; [ Code] (define acc2 (make-account 100))
; [ Eval] (make-account 100) (env: GLOBAL)
; [Apply] <proc#1> @ 100
; [Frame] E21 => balance: 100, env -> GLOBAL
; [ Eval] (define withdraw ...)... dispatch (env: E21)
; [ Proc] <proc#5> => params: amount, body: ..., env -> E21
; [ Proc] <proc#6> => params: amount, body: ..., env -> E21
; [ Proc] <proc#7> => params: m, body: ..., env -> E21
; [Frame] E21 <= withdraw: <proc#5>
; [Frame] E21 <= deposit: <proc#6>
; [Frame] E21 <= dispatch: <proc#7>
; [Frame] GLOBAL <= acc2: <proc#7>
;
; [ Code] ((acc1 'deposit) 40)
; [ Eval] (acc1 'deposit) (env: GLOBAL)
; [Apply] <proc#4> @ 'deposit
; [Frame] E12 => m: 'deposit, env -> E11 (env of <proc#4>)
; [ Eval] (cond ... m withdraw deposit ...) (env: E12)
; [ Eval] m -> 'deposit (env: E12)
; [ Eval] deposit -> <proc#3> (env: E11)
; [  Rst] <- <proc#3>
; [ Eval] (<proc#3> 40) (env: GLOBAL)
; [Apply] <proc#3> @ 40
; [Frame] E13 => amount: 40, env -> E11 (env of <proc#3>)
; [ Eval] (set! ... balance amount ...) (env: E13)
; [ Eval] balance -> 50 (env: E11), amount -> 40 (env: E13)
; [  Rst] balance <- 90 (env: E11), <- 90
;
; [ Code] ((acc2 'deposit) 40)
; [ Eval] (acc2 'deposit) (env: GLOBAL)
; [Apply] <proc#7> @ 'deposit
; [Frame] E22 => m: 'deposit, env -> E21 (env of <proc#7>)
; [ Eval] (cond ... m withdraw deposit ...) (env: E22)
; [ Eval] m -> 'deposit (env: E22)
; [ Eval] deposit -> <proc#6> (env: E21)
; [  Rst] <- <proc#6>
; [ Eval] (<proc#6> 40) (env: GLOBAL)
; [Apply] <proc#6> @ 40
; [Frame] E23 => amount: 40, env -> E21 (env of <proc#6>)
; [ Eval] (set! ... balance amount ...) (env: E23)
; [ Eval] balance -> 100 (env: E21), amount -> 40 (env: E23)
; [  Rst] balance <- 140 (env: E21), <- 140
))


;;; 3.12
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

((lambda ()
(define (append! x y)
  (set-cdr! (last-pair x) y))

(hlog "ex-3.12")
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(displayln z)  ; => (a b c d)
(displayln (cdr x))  ; => (b)

(define w (append! x y))

(displayln w)  ; => (a b c d)
(displayln (cdr x))  ; => (b c d)
(tlog)
))


;;; 3.13
(define (sub-list lst n)
  (cond
    [(or (null? lst) (= n 0)) '()]
    [else (cons (car lst) (sub-list (cdr lst) (dec n)))]))

((lambda ()
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
; (a b c a b c)

(mlog "ex-3.13"
(sub-list z 7)
)
))


;;; 3.14
((lambda ()
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ([temp (cdr x)])
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

(mlog "ex-3.14"
v  ; => (a)
w  ; => (d c b a)
)
))


;;; 3.15
; ignored


;;; 3.16 ~ 3.19
; TODO


;;; 3.20
; ignored


;;; 3.21
(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error 'front-queue "empty queue")
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (cons item '())])
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
  (if (empty-queue? queue)
    (error 'delete-queue! "empty queue")
    (begin
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))

(define (queue->list queue)
  (define (iter lst)
    (if (null? lst)
      '()
      (cons (car lst) (iter (cdr lst)))))
  (iter (front-ptr queue)))

((lambda ()
(hlog "ex-3.21")
(define q1 (make-queue))
(displayln (insert-queue! q1 'a))
(displayln (queue->list q1))
(displayln (insert-queue! q1 'b))
(displayln (queue->list q1))
(displayln (delete-queue! q1))
(displayln (queue->list q1))
(displayln (delete-queue! q1))
(displayln (queue->list q1))
(tlog)
))


;;; 3.22
((lambda ()
(define (make-queue)
  (let ([front-ptr '()] [rear-ptr '()])
    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
        (error 'front-queue "empty queue")
        (car front-ptr)))

    (define (insert-queue! item)
      (let ([new-pair (cons item '())])
        (cond
          [(empty-queue?)
             (set! front-ptr new-pair)
             (set! rear-ptr new-pair)]
          [else
             (set-cdr! rear-ptr new-pair)
             (set! rear-ptr new-pair)])))

    (define (delete-queue!)
      (if (empty-queue?)
        (error 'delte-queue! "empty queue")
        (set! front-ptr (cdr front-ptr))))

    (define (queue->list)
      (define (iter lst)
        (if (null? lst)
          '()
          (cons (car lst) (iter (cdr lst)))))
      (iter front-ptr))

    (define (dispatch m)
      (cond
        [(eqv? m 'empty-queue?) (empty-queue?)]
        [(eqv? m 'front-queue) (front-queue)]
        [(eqv? m 'insert-queue!) insert-queue!]
        [(eqv? m 'delete-queue!) (delete-queue!)]
        [(eqv? m 'queue->list) (queue->list)]
        [else (error 'make-queue "unknown message" m)]))
    dispatch))

(hlog "ex-3.22")
(define q1 (make-queue))
(displayln (q1 'empty-queue?))  ; => #t
((q1 'insert-queue!) 'a)
(displayln (q1 'empty-queue?))  ; => #f
(displayln (q1 'front-queue))  ; => a
(displayln (q1 'queue->list))  ; => (a)
((q1 'insert-queue!) 'b)
(displayln (q1 'front-queue))  ; => a
(displayln (q1 'queue->list))  ; => (a b)
(q1 'delete-queue!)
(displayln (q1 'empty-queue?))  ; => #f
(displayln (q1 'front-queue))  ; => b
(displayln (q1 'queue->list))  ; => (b)
(q1 'delete-queue!)
(displayln (q1 'empty-queue?))  ; => #t
(displayln (q1 'queue->list))  ; => ()
(tlog)
))


;;; 3.23
((lambda ()
(define (make-deque) (cons '() '()))

(define (empty-deque? queue) (null? (car queue)))

(define (front-deque queue)
  (if (empty-deque? queue)
     (error 'front-deque "empty deque")
     (car (car (car queue)))))

(define (rear-deque queue)
  (if (empty-deque? queue)
    (error 'rear-deque "empty deque")
    (car (car (cdr queue)))))

(define (front-insert-deque! queue item)
  (let ([new-pair (cons (cons item '()) (car queue))])
    (cond
      [(empty-deque? queue)
         (set-car! queue new-pair)
         (set-cdr! queue new-pair)]
      [else
         (set-car! queue new-pair)])))

(define (rear-insert-deque! queue item)
  (let ([new-pair (cons (cons item (cdr queue)) '())])
    (cond
      [(empty-deque? queue)
         (set-car! queue new-pair)
         (set-cdr! queue new-pair)]
      [else
        (set-cdr! (cdr queue) new-pair)
        (set-cdr! queue new-pair)])))

(define (front-delete-deque! queue)
  (cond
    [(empty-deque? queue)
       (error 'front-delete-deque! "empty deque")]
    [(eqv? (car queue) (cdr queue))
       (set-car! queue '())
       (set-cdr! queue '())]
    [else
      (set-car! queue (cdr (car queue)))]))

(define (rear-delete-deque! queue)
  (cond
    [(empty-deque? queue)
       (error 'rear-delete-deque! "empty deque")]
    [(eqv? (car queue) (cdr queue))
       (set-car! queue '())
       (set-cdr! queue '())]
    [else
       (let* ([tail (cdr queue)]
              [tail-prev (cdr (car tail))])
         (set-cdr! queue tail-prev)
         (set-cdr! tail-prev '()))]))

(define (deque->list queue)
  (define (iter lst)
    (if (null? lst)
      '()
      (cons (car (car lst)) (iter (cdr lst)))))
  (iter (car queue)))

(define (test queue)
  (if (empty-deque? queue)
    (displayln #t)
    (begin
      (displayln (front-deque queue))
      (displayln (rear-deque queue))
      (displayln (deque->list queue)))))

(define q (make-deque))

(hlog "ex-3.23")
(test q)  ; => #t
(front-insert-deque! q 'a)
(test q)  ; => (a)
(front-insert-deque! q 'b)
(test q)  ; => (b a)
(rear-insert-deque! q 'c)
(test q)  ; => (b a c)
(front-delete-deque! q)
(test q)  ; => (a c)
(rear-delete-deque! q)
(test q)  ; => (a)
(rear-delete-deque! q)
(test q)  ; => #t
(tlog)
))


;;; 3.24
((lambda ()
(define (make-table same-key?)
  (let ([local-table (list '*table*)])
    (define (assoc key records)
      (cond
        [(null? records) #f]
        [(same-key? key (car (car records))) (car records)]
        [else (assoc key (cdr records))]))

    (define (lookup key)
      (let ([record (assoc key (cdr local-table))])
        (if record
          (cdr record)
          #f)))

    (define (insert! key value)
      (let ([record (assoc key (cdr local-table))])
        (if record
          (set-cdr! record value)
          (set-cdr!
            local-table
            (cons (cons key value) (cdr local-table))))))
    
    (define (dispatch m)
      (cond
        [(eqv? m 'lookup) lookup]
        [(eqv? m 'insert!) insert!]
        [(eqv? m 'table->list) local-table]
        [else (error 'make-table "unknown message" m)]))
    dispatch))

(define (same-key? key1 key2)
  (<= (abs (- key1 key2)) 0.1))

(define t (make-table same-key?))

(hlog "ex-3.24")
((t 'insert!) 1.0 1.0)
((t 'insert!) 2.0 2.0)
((t 'insert!) 3.0 3.0)
(displayln (t 'table->list))
(displayln ((t 'lookup) 4.0))  ; => #f
(displayln ((t 'lookup) 1.95))  ; => 2.0
(displayln ((t 'lookup) 1.85))  ; => #f
((t 'insert!) 1.95 1.95)
(displayln (t 'table->list))
(tlog)
))

;;; 3.25
((lambda ()
(define (make-table) (list '*table*))

(define (assoc key records)
  (cond
    [(null? records) #f]
    [(eqv? key (car (car records))) (car records)]
    [else (assoc key (cdr records))]))

(define (lookup key-list table)
  (if (null? key-list)
    #f
    (let* ([key (car key-list)]
           [sub-table (assoc key (cdr table))])
      (cond
        [(not sub-table) #f]
        [(null? (cdr key-list)) (cdr sub-table)]
        [else (lookup (cdr key-list) sub-table)]))))

(define (insert! key-list value table)
  (define (gen-node klist val)
    (let ([key (car klist)])
      (cond
        [(null? (cdr klist)) (cons key val)]
        [else (cons key (list (gen-node (cdr klist) val)))])))
  (if (null? key-list)
    #f
    (let ([node (gen-node key-list value)])
      (cond
        [(null? (cdr table))
           (set-cdr! table (cons node (cdr table)))]
        [(pair? (cdr table))
           (let* ([key (car key-list)]
                  [sub-table (assoc key (cdr table))])
             (cond
               [(not sub-table)
                  (set-cdr! table (cons node (cdr table)))]
               [(null? (cdr key-list)) (set-cdr! sub-table value)]
               [else (insert! (cdr key-list) value sub-table)]))]
        [else
           (set-cdr! table (list node))]))))

(define t (make-table))
(insert! (list 'a01) 1 t)
(insert! (list 'a02) 2 t)
(insert! (list 'a02 'b01) 101 t)
(insert! (list 'a03 'b02 'b03) 123 t)

(mlog "ex-3.25"
  t
  (lookup (list 'a02 'b01) t)  ; => 101
  (lookup (list 'a01) t)  ; => 1
  (lookup (list 'a02) t)  ; => ((b01 . 101))
  (lookup (list 'a03 'b02 'b01) t)  ; => #f
  (lookup (list 'a03 'b03) t)  ; => #f
  (lookup (list 'a03 'b02 'b03) t)  ; => 123
)
))


;;; 3.26
((lambda ()
(define (make-bt-table) (list '() '() '() '()))

(define (root-key table) (car table))
(define (root-val table) (car (cdr table)))
(define (left-branch table) (car (cdr (cdr table))))
(define (right-branch table) (car (cdr (cdr (cdr table)))))

(define (set-key! table key) (set-car! table key))
(define (set-val! table val) (set-car! (cdr table) val))
(define (set-left-branch! table branch) (set-car! (cdr (cdr table)) branch))
(define (set-right-branch! table branch) (set-car! (cdr (cdr (cdr table))) branch))

(define (lookup key table)
  (if (null? table)
    #f
    (let ([rkey (root-key table)])
      (cond
        [(< key rkey) (lookup key (left-branch table))]
        [(> key rkey) (lookup key (right-branch table))]
        [else (root-val table)]))))

(define (insert! key value table)
  (let* ([rkey (root-key table)]
         [lb (left-branch table)]
         [rb (right-branch table)]
         [node (list key value '() '())])
    (cond
      [(null? rkey)
         (set-key! table key)
         (set-val! table value)]
      [(< key rkey)
         (cond
           [(null? lb) (set-left-branch! table node)]
           [else (insert! key value lb)])]
      [(> key rkey)
         (cond
           [(null? rb) (set-right-branch! table node)]
           [else (insert! key value rb)])]
      [else (set-val! table value)])))

(define bt (make-bt-table))
(insert! 10 10 bt)
(insert! 20 20 bt)
(insert! 6 6 bt)
(insert! 10 11 bt)
(insert! 3 3 bt)
(insert! 13 13 bt)
(insert! 1 1 bt)

(mlog "ex-3.26"
  bt
  (lookup 20 bt)  ; => 20
  (lookup 10 bt)  ; => 11
  (lookup 1 bt)   ; => 1
  (lookup 13 bt)  ; => 13
  (lookup 15 bt)  ; => #f
)
))


;;; 3.27
; ignored


;;; 3.28 ~ 3.37
; TODO


;;; 3.38
; 45, 35, 50, 40 , ...
; ignored


;;; 3.39 ~ 3.49
; TODO