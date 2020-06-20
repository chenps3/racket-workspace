#lang sicp

(define (f g) (g 2))

(define (square x)
  (* x x))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)
;1 代换为(f 2)
;2 代换为(2 2)
;3 报错，因为左边的2不是一个过程