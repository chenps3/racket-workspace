#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (even? k)
  (= (remainder k 2) 0))

;假设了n是偶数
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coef k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (y k) (f (+ a (* k h))))
  (define (next a) (+ a 1))
  (define (f1 k) (* (coef k) (y k)))
  (/ (* h (sum-2 f1 0 next n)) 3))

(define (cube a)
  (* a a a))

;对照组
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2.0)) add-dx b)))
