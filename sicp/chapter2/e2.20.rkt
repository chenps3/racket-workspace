#lang sicp
;迭代计算过程实现，
(define (same-parity a . b)
  (define (iter l acc)
      (if (null? l)
          acc
          (iter (cdr l)
                (if (same-parity? a (car l))
                    (append acc (list (car l)))
                    acc))))
  (cons a (iter b nil)))


(define (even? a)
  (= (remainder a 2) 0))

(define (same-parity? a b)
    (even? (- a b)))


(same-parity 1 2 3 4 5 6 7)