#lang sicp
(define (bigger-sum a b c)
  (define bigger (if (> a b) a b))
  (define smaller (if (> a b) b a))
  (+ bigger (if (> c smaller) c smaller)))
