#lang sicp
(define (last-pair l)
  (let ((tail (cdr l)))
    (if (null? tail)
        l`
        (last-pair tail))))

(define l1 (list 1 2 3 4))

(last-pair l1)