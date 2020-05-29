#lang sicp

;求幂，递归计算过程 O(n)时间 O(n)空间
(define (expt1 b n)
  (if (= n 0)
      1
      (* b (expt1 b (- n 1)))))

;求幂，迭代计算过程 O(n)时间 O(1)空间
(define (expt2 b n)
  (define (expt2-iter e count)
    (if (= count 0)
        e
        (expt2-iter (* e b) (- count 1))))
  (expt2-iter 1 n))

;二分法 递归计算过程
(define (fast-expt b n)
  (if (= n 0) 1
      (if (even? n)
          (square (fast-expt b (/ n 2)))
          (* b (fast-expt b (- n 1))))))

(define (even? a)
  (= (remainder a 2) 0))

(define (square x)
  (* x x))