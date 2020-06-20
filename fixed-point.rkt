#lang sicp

(define (close-enough? a b) 
  (< (abs (- a b)) 0.00001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? next guess)
              next
              (try next))))
  (try first-guess))

