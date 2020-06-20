#lang sicp

(define (close-enough? a b)
  (< (abs (- a b)) 0.00001))

(define (average a b)
  (/ (+ a b) 2))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (power x n)
  (define (iter x n result)
    (if (= n 0)
        result
        (iter x (- n 1) (* result x))))
  (iter x n 1))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (lambda (x)
    ((if (= n 1)
         f
         (compose f (repeated f (- n 1))))
     x)))

;不能用1作为猜测值
(define (root-n x n)
  (define (repeat-times) (floor (/ (log n) (log 2))))
  (define (f y) (/ x (power y (- n 1))))
  (fixed-point ((repeated average-damp (repeat-times)) f)
               1.0))
