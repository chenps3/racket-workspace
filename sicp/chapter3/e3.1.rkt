#lang sicp

(define (make-accumulator init)
  (lambda (add)
    (+ init add)))

(define a (make-accumulator 5))
(define b (make-accumulator 1))

(a 1)
(a 2)
(b 1)
(b 2)
