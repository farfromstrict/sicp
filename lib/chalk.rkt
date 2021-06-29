#lang racket
(provide chalk)


(define color-table
  '((default 9) (white 67)
    (black 0) (red 1) (green 2) (yellow 3) (blue 4) (magenta 5) (cyan 6) (light-gray 7)
    (dark-gray 60) (light-red 61) (light-green 62) (light-yellow 63)
    (light-blue 64) (light-magenta 65) (light-cyan 66)))

;; blink/hidden may NOT work
(define effect-table
  '((none #f) (bold 1) (dim 2) (underlined 4) (blink 5) (reverse 7) (hidden 8)))

(define (get-num symbol table)
  (let ([record (assoc symbol table)])
    (if record
      (cadr record)
      (error 'get-num "symbol not support: ~a" symbol))))

(define (get-fg-num color) (+ 30 (get-num color color-table)))
(define (get-bg-num color) (+ 40 (get-num color color-table)))
(define (get-effect-num effect) (get-num effect effect-table))

(define (chalk-transfer str fg-num bg-num effect-num)
  (let* ([prefix (format "~a;~a" fg-num bg-num)]
         [options (if effect-num
                    (format "~a;~a" prefix effect-num)
                    prefix)])
    (format "\033[~am~a\033[0m" options str)))

(define (chalk #:fg-color [fg-color 'default] #:bg-color [bg-color 'default] #:effect [effect 'none]
               fmt . args)
  (let ([fg-num (get-fg-num fg-color)] [bg-num (get-bg-num bg-color)]
        [effect-num (get-effect-num effect)])
    (chalk-transfer (apply format (cons fmt args)) fg-num bg-num effect-num)))