#lang sicp

;f定义：
;如果n < 3, f(n) = n
;如果n >= 3, f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

;递归计算过程
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))

;迭代计算过程
(define (f2 n)
  (define (f2-iter a b c count)
    (if (< n 3)
      n
      (f2-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (if (< n 3)
      n
      (f2-iter 4 11 25 n)))
;f(3) = 4
;f(4) = 11
;f(5) = 25
(define (f2-iter a b c count)
  (if (= count 3)
      a
      (f2-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

