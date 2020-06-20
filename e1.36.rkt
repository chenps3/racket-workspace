#lang sicp

(define (close-enough? a b) 
  (< (abs (- a b)) 0.00001))

(define (fixed-point f first-guess)
  (define (try guess)
      (newline)
      (display guess)
    (let ((next (f guess)))
          (if (close-enough? next guess)
              next
              (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

;33次
(display "不采用平均阻尼")
(define root1 (fixed-point (lambda (x) (/ (log 1000) (log x))) 10))
(newline)
;10次
(display "采用平均阻尼")
(define root2 (fixed-point (lambda (x) (average (/ (log 1000) (log x)) x))
                           10))