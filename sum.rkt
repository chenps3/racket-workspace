#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (cube a)
  (* a a a))

(define (inc a)
  (+ a 1))

(define (sum-cube a b)
  (sum cube a inc b))

(define (identity a) a)

(define (sum-integer a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2.0)) add-dx b)))