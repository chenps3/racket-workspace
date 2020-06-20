#lang sicp
(define (average a b)
  (/ (+ a b) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (pos? x)
  (> x 0))

(define (neg? x)
  (< x 0))

;二分法查找函数零点
(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((pos? test-value) (search f neg-point mid-point))
                ((neg? test-value) (search f mid-point pos-point))
                (else mid-point))))))

;如果给定a b对应的值同号，报错
(define (zero-point f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (neg? a-value) (pos? b-value))
           (search f a b))
          ((and (pos? a-value) (neg? b-value))
           (search f b a))
          (else (error "值不是异号" a b)))))

;test
(zero-point (lambda (x) (+ 1 (* x 2))) -1 1)
;(zero-point (lambda (x) (+ 1 (* x 2))) 1 2)









