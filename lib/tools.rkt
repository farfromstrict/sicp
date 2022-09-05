#lang racket
(provide average int/list diff-ratio mlist mlist->list list->mlist list->mlist-rec repeat arrange)

(define (average e . w)
  (let ([sum (+ e (apply + w))]
        [cnt (+ 1 (length w))])
    (/ sum cnt)))

(define (int/list low high [step 1])
  (let loop ([n low] [rst '()])
    (if (> n high)
        (reverse rst)
        (loop (+ n step) (cons n rst)))))

(define (diff-ratio v1 v2)
  (let ([sum (+ v1 v2)])
    (cond
      [(= sum 0) (abs v1)]
      [else (abs (/ (- v1 v2) (/ sum 2)))])))

(define (mlist . w)
  (cond
    [(null? w) w]
    [else (mcons (car w) (apply mlist (cdr w)))]))

(define (mlist->list mlst)
  (cond
    [(null? mlst) '()]
    [else (cons (mcar mlst) (mlist->list (mcdr mlst)))]))

(define (list->mlist lst)
  (cond
    [(or (null? lst) (mpair? lst)) lst]
    [else
     (mcons (car lst) (list->mlist (cdr lst)))]))

(define (list->mlist-rec lst)
  (cond
    [(or (null? lst) (mpair? lst)) lst]
    [else
     (let ([head (car lst)])
       (cond
         [(pair? head) (mcons (list->mlist head) (list->mlist (cdr lst)))]
         [else (mcons head (list->mlist (cdr lst)))]))]))

(define (repeat n proc)
  (let loop ([t n])
    (cond
      [(> t 0) (proc) (loop (- t 1))])))

(define (arrange lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) (list lst)]
    [else
     (foldr
       (lambda (x rst)
         (let ([rest (filter (lambda (y) (not (eq? x y))) lst)])
           (append (map (lambda (z) (cons x z)) (arrange rest)) rst)))
       '()
       lst)]))
