#lang racket
(define (t x)
  (map
    (lambda (y)
      (cond
        [(string->number y) (string->number y)]
        [else y]))
    (string-split x)))
(define (f x)
  (cond
  [(empty? x) (list)]
  [else (cdr x)]))
(define (p i s)
  (match i
    [(list) s]
    [(list "+" _ ...) (p (f i) (cons (+ (cadr s) (car s)) (cddr s)))]
    [(list "-" _ ...) (p (f i) (cons (- (cadr s) (car s)) (cddr s)))]
    [(list "*" _ ...) (p (f i) (cons (* (cadr s) (car s)) (cddr s)))]
    [(list "/" _ ...) (p (f i) (cons (/ (cadr s) (car s)) (cddr s)))]
    [_ (p (f i) (cons (car i) s))]
    )
)
(p (t (read-line)) '())
