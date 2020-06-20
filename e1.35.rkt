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

;方程 X = 1 + 1/X  两边同乘以X得
;X² = X + 1，满足这个方程的值就是黄金分割率
(define gold (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))