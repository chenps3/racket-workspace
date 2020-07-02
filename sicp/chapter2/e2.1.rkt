#lang sicp

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (same-sign? n d) + -)))
    (cons (sign (abs (/ n g)))
          (abs (/ d g)))))

(define (same-sign? a b)
  (or (and (> a 0) (> (+ a b) a))
      (and (< a 0) (< (+ a b) a))
      (= a 0)
      (= b 0)))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 1 3))
(print-rat (make-rat -2 3))
(print-rat (make-rat 2 -3))
(print-rat (make-rat 0 -3))
