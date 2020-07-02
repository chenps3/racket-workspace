#lang sicp
(define (power x n)
  (define (iter x n result)
    (if (= n 0)
        result
        (iter x (- n 1) (* x result))))
  (iter x n 1))

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (extract-expt x a)
  (define (iter x a result)
    (if (= (remainder x a) 0)
        (iter (/ x a) (+ result 1))
        result))
  (iter x a 0))

(define (car x)
  (extract-expt x 2))

(define (cdr x)
  (extract-expt x 3))