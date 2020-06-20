#lang sicp

(define (good-enough old-guess new-guess)
  (< (/ (abs (- new-guess old-guess)) old-guess) 0.01))

(define (abs x)
  (if (< x 0) (- x) x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square x) (* x x))

(define (root-iter guess x)
  (if (good-enough guess (improve guess x))
      (improve guess x)
      (root-iter (improve guess x) x)))

(define (root x)
  (root-iter 1.0 x))

;块结构
(define (root-v2 x)
  (define (good-enough old-guess new-guess)
    (< (/ (abs (- new-guess old-guess)) old-guess) 0.01))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (root-iter guess)
    (if (good-enough guess (improve guess))
        (improve guess)
        (root-iter (improve guess))))
  (root-iter 1.0))