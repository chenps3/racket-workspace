#lang sicp

;这个版本是个递归计算过程
(define (sum-1 term a next b)
  (if (> a b)
      0
      (+ (term a) (sum-1 term (next a) next b))))

;迭代计算过程
(define (sum-2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))