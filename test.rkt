#lang sicp
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(define (abs a)
  (cond ((>= a 0) a)
        (else (- a))))

(define square-v2 (lambda (x) (* x x)))

(define (test x)
  (+ (let ((x 3))
       (+ x (* x 10)))
     x))