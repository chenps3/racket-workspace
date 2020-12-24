#lang sicp
;直角坐标形式
;实部
(define (real-part-1 z) (car z))
;虚部
(define (imag-part-1 z) (cdr z))
;模
(define (square x) (* x x))
(define (magnitude-1 z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
;幅角
(define (angle-1 z) (atan (imag-part-1 z) (real-part-1 z)))
;根据实部虚部构造
(define (make-from-real-imag-1 x y) (cons x y))
;根据模和幅角构造
(define (make-from-mag-ang-1 r a) (cons (* r (cos a)) (* r (sin a))))

;极坐标形式
;实部
(define (real-part-2 z) (* (magnitude-2 z) (cos (angle-2 z))))
;虚部
(define (imag-part-2 z) (* (magnitude-2 z) (sin (angle-2 z))))
;模
(define (magnitude-2 z) (car z))
;幅角
(define (angle-2 z) (cdr z))
;根据实部虚部构造
(define (make-from-real-imag-2 x y)
  (cons (sqrt (+ (square x) (square y))) (atan y x)))
;根据模和幅角构造
(define (make-from-mag-ang-2 r a)
  (cons r a))


