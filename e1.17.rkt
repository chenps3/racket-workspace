#lang sicp
(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (even? a)
  (= (remainder a 2) 0))

(define (multiply a b)
  (cond ((= b 0) 0)
        ((< b 0) (- (multiply a (- b))))
        ((even? b) (multiply (double a) (halve b)))
        (else (+ a (multiply a (- b 1))))))


