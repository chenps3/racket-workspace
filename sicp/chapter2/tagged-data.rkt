#lang sicp
;生成带标志的数据对象
(define (attach-tag type-tag contents)
  (cons type-tag contents))
;选择函数
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagger datum -- CONTENTS" datum)))

;判断复数是直角坐标表示还是极坐标表示
(define (rect? z) (eq? 'rect (type-tag z)))
(define (polar? z) (eq? 'polar (type-tag z)))

;基于类型标志的复数表示
(define (square x) (* x x))
;直角坐标实现
(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))
(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z)) (square (imag-part-rect z)))))
(define (angle-rect z) (atan (imag-part-rect z) (real-part-rect z)))
(define (make-from-real-imag-rect x y)
  (attach-tag 'rect (cons x y)))
(define (make-from-mag-ang-rect r a)
  (attach-tag 'rect (cons (* r (cos a)) (* r (sin a)))))
;极坐标
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;通用选择函数
(define (real-part z)
  (cond ((rect? z) (real-part-rect z))
        ((polar? z) (real-part-polar z))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rect? z) (imag-part-rect z))
        ((polar? z) (imag-part-polar z))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rect? z) (magnitude-rect z))
        ((polar? z) (magnitude-polar z))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rect? z) (angle-rect z))
        ((polar? z) (angle-polar z))
        (else (error "Unknown type -- ANGLE" z))))

;复数运算
(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))








