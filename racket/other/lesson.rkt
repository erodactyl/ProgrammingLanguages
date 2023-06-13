#lang racket

;; fibonacci
(define (fib n) (
  if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)

;; sum of arbitrarily nested lists of numbers
(define (sum xs) (
    if (null? xs)
      0
      (if (number? (car xs))
        (+ (car xs) (sum (cdr xs)))
        (+ (sum (car xs)) (sum (cdr xs)))
      )
  )
)

;; get max of a list of numbers
(define (max-of-list xs)
  (cond [(null? xs) (error "can't get max of empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([max-of-rest (max-of-list (cdr xs))])
              (if (< max-of-rest (car xs)) (car xs) max-of-rest))]))

