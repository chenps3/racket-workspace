#lang sicp

(define (fact n)
  (define fact-iter (lambda (product count max-count)
                      (if (> count max-count)
                          product
                          (fact-iter (* count product) (+ count 1) max-count))))
  (fact-iter 1 1 n))

