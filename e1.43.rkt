#lang sicp
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

(define (repeated f n)
  (lambda (x)
    ((if (= n 1)
         f
         (compose f (repeated f (- n 1))))
     x)))

((repeated square 3) 3)