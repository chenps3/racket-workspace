#lang sicp

(define (close-enough? a b)
  (< (abs (- a b)) 0.00001))

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

(define (cube x)
  (* x x))

;求导
(define dx 0.00001)
(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

;不动点
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
  (try first-guess))

;平均阻尼转换
(define (average-damp f)
  (lambda (x) (average (f x) x)))

;开平方法1，对Y=X/Y平均阻尼转换后求不动点
(define (sqrt1 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1))

;牛顿变换：如果x->g(x)可微，g(x)=0的一个解就是x->f(x)的不动点
;其中f(x) = x - (g(x)/Dg(x)),Dg(x)就是g(x)的导数
(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

;牛顿法求零点
(define (newton-method g guess)
  fixed-point (newton-transform g guess))

;牛顿法开方,即求f(y) = y²-x的零点
(define (sqrt2 x)
  (newton-method (lambda (y) (- (square y) x)) 1))

;e1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))



