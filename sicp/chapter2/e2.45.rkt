#lang sicp

(define (split p1 p2)
  (lambda (painter)
    (p1 painter (p2 painter painter))))

