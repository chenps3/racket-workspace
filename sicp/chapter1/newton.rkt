#lang sicp

;求平方根
(define (sqrt x)
  (sqrt-iter 1.0 x))

;牛顿迭代法
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

;计算y与x/y的平均值
(define (improve guess x)
  (average (/ x guess) guess))

;求平均值
(define (average x y)
  (/ (+ x y) 2))

;猜测值是否足够接近
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;平方
(define (square x)
  (* x x))

;绝对值
(define (abs x)
  (if (< x 0) (- x) x))

;练习1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                         x)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

;练习1.7
(define (good-enough-v2 new-guess old-guess)
  (< (/ (abs (- new-guess old-guess)) old-guess) 0.01))
(define (sqrt-iter-v2 guess x)
  (if (good-enough-v2 (improve guess x) guess)
      (improve guess x)
      (sqrt-iter-v2 (improve guess x) x)))
(define (sqrt-v2 x)
  (sqrt-iter-v2 1.0 x))