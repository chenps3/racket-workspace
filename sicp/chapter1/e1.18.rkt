#lang sicp
(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (even? a)
  (= (remainder a 2) 0))

(define (multiply x y)
  (define (multiply-iter prod a b)
    (cond ((= b 0) prod)
          ((even? b) (multiply-iter prod (double a) (halve b)))
          (else (multiply-iter (+ prod a) a (- b 1)))))
  (multiply-iter 0 x y))