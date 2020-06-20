#lang sicp

;accumulate 定义，递归计算过程
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;accumulate 定义，迭代计算过程
(define (accumulate1 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;使用accumulate重新定义sum
(define (sum f a b next)
  (accumulate1 + 0 f a next b))

;使用accumulate重新定义product
(define (product f a b next)
  (accumulate1 * 1 f a next b))

;测试sum
(define (sum-int a b)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (sum identity a b next))

;测试product
(define (fact n)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (product identity 1 n next))