#lang sicp
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))



(define (smooth f x)
  (define average (lambda (a b c) (/ (+ a b c) 3)))
  (let ((dx 0.00001))
    (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx))))))


(define (smooth-n n)
  (repeated smooth n))


