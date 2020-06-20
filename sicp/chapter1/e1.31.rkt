#lang sicp

;a 递归计算过程
(define (product1 f a b next)
  (if (> a b)
      1
      (* (f a) (product1 f (next a) b next))))

;b 迭代计算过程
(define (product2 f a b next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (f a)))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (next a) (+ a 1))
  (product1 identity 1 n next))

(define (pi n)
  (define (next a) (+ a 1))
  (* 4 (product1 pi-term 1 n next)))

(define (pi-term k)
  (if (even? k)
      (/ (+ k 2) (+ k 1))
      (/ (+ k 1) (+ k 2))))

(define (even? a)
  (= (remainder a 2) 0))
