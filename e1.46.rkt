#lang sicp
(define (interative-improve f-good-enough? f-improve)
  (lambda (guess)
    (if (f-good-enough? guess)
        guess
        ((interative-improve f-good-enough? f-improve)
         (f-improve guess)))))

(define (close-enough? a b)
  (< (abs (- a b)) 0.00001))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x)
  (* x x))

(define (fixed-point f first-guess)
  (define (good-enough? guess) (close-enough? (f guess) guess))
  ((interative-improve good-enough? (average-damp f)) first-guess))

(define (sqrt x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
      (average guess (/ x guess)))
    ((interative-improve good-enough? improve) 1.0))

(define (sqrt1 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))