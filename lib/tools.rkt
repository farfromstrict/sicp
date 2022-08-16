#lang racket
(provide average int/list join diff-ratio mlist list->mlist repeat)

(define (average e . w)
  (let ([sum (+ e (apply + w))]
        [cnt (+ 1 (length w))])
    (/ sum cnt)))

(define (int/list low high [step 1])
  (let loop ([n low] [rst '()])
    (if (> n high)
        (reverse rst)
        (loop (+ n step) (cons n rst)))))

(define (join lst delim)
  (cond
    [(null? lst) ""]
    [else
     (let loop ([lst (cdr lst)] [rst (format "~a" (car lst))])
       (cond
         [(null? lst) rst]
         [else (loop (cdr lst) (format "~a~a~a" rst delim (car lst)))]))]))

(define (diff-ratio v1 v2)
  (let ([sum (+ v1 v2)])
    (cond
      [(= sum 0) (abs v1)]
      [else (abs (/ (- v1 v2) (/ sum 2)))])))

(define (mlist . w)
  (cond
    [(null? w) w]
    [else (mcons (car w) (apply mlist (cdr w)))]))

(define (list->mlist lst)
  (cond
    [(null? lst) lst]
    [else
     (let ([head (car lst)])
       (cond
         [(pair? head) (mcons (list->mlist head) (list->mlist (cdr lst)))]
         [else (mcons head (list->mlist (cdr lst)))]))]))

(define (repeat n proc)
  (let loop ([t n])
    (cond
      [(> t 0) (proc) (loop (- t 1))])))