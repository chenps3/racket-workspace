#lang sicp
(define (make-interval a b)
  (cons a b))

(define (display-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))

;e2.7,这样构造时就不用关心哪个大，哪个小
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;e2.8 x-y = x + (- y)
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (lower-bound y))
                               (- (upper-bound y)))))

;e2.9
(define (width x)
  (/ (- (upper-bound x) (lower-bound x))
     2.0))
;加法
;令区间x=[lx,ux],y=[ly,uy]
;width(x+y)
;= width([lx+ly,ux+uy])
;= ((ux+uy)-(lx+ly))/2
;= (ux-lx)/2 + (uy-ly)/2
;= width(x) + width(y)

;减法
;令区间x=[lx,ux],y=[ly,uy]
;width(x-y)
;= width ([lx-ly,ux-uy])
;= ((ux-uy)-(lx-ly))/2
;= (ux-lx)/2-(uy-ly)/2
;= width(x) - width(y)

;乘法
;令区间x=[0,10],y=[2,20]
;区间x*y = [0,200]
;width(x) = 5
;width(y) = 9
;width(x*y) = 100

;再令x=[10,20],y=[2,20]
;区间x*y = [20,400]
;width(x) = 5
;width(y) = 9
;width(x*y) = 190

;x宽度y宽度都没有变，但是x*y宽度变了，除法同理

;e2.10
(define (div-interval-v2 x y)
  (if (cross-zero? y)
      (error "error：y cross zero")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (cross-zero? x)
  (or (= 0 (upper-bound x))
      (= 0 (lower-bound x))
      (and (< (lower-bound x) 0)
           (> (upper-bound x) 0))))

(define i1 (make-interval 5 6))
(define i2 (make-interval -1 1))

;(display-interval (add-interval i1 i2))
;(display-interval (div-interval-v2 i1 i2))


;e2.11
(define (sign-of-interval x)
  (let ((lx (lower-bound x))
        (ux (upper-bound x)))
    (cond ((and (< lx 0) (< ux 0)) -1)
          ((and (> lx 0) (> ux 0)) 1)
          (else 0))))
(define (mul-interval-v2 x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y))
        (xs (sign-of-interval x))
        (ys (sign-of-interval y)))
    (cond ((< xs 0)
           (cond ((< ys 0)
                  (make-interval (* ux uy) (* lx ly))) ;- - - -
                 ((= ys 0)
                  (make-interval (* lx uy) (* lx ly))) ;- - - +
                 ((> ys 0)
                  (make-interval (* lx uy) (* ux ly))))) ;- - + +
          ((= xs 0)
           (cond ((< ys 0)
                  (make-interval (* ux ly) (* lx ly))) ; - + - -
                 ((= ys 0)
                  (make-interval (min (* lx uy) (* ux ly))
                                 (max (* lx ly) (* ux uy))) ; - + - +
                 ((> ys 0)
                  (make-interval (* lx uy) (* ux uy))))) ; - + + +
          ((> xs 0)
           (cond ((< ys 0)
                  (make-interval (* ux ly) (* lx uy))) ; + + - -
                 ((= ys 0)
                  (make-interval (* ux ly) (* ux uy))) ; + + - +
                 ((> ys 0)
                  (make-interval (* lx ly) (* ux uy))))))))) ; + + + +

;e2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center x)
  ((/ (+ (upper-bound x) (lower-bound x)) 2)))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

(define (percent x)
  (let ((w (width x))
        (c (center x)))
    (/ w c)))


;e2.13
;区间a=[ca-ta,ca+ta] 区间b=[cb-tb,cb+tb]
;假设都是正数，那么有
;a*b = [(ca-ta)*(cb-tb),(ca+ta)*(cb+tb)]
;    = [ca*cb - ca*tb - ta*cb + ta*tb, ca*cb + ca*tb +ta*cb +ta*tb]
;ta*tb很小，所以可以忽略
;    = (ca*cb) [1 - tb/cb - ta/ca， 1 + tb/cb + ta/ca]
;    = (ca*cb) [1 - pb - pa , 1 + pb + pa]

;e2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-center-percent 10 0.0001))
(define B (make-center-percent 20 0.0002))

(define C (div-interval A A))
(define D (div-interval A B))
(define E (par1 A B))
(define F (par2 A B))
(display-interval C)
(display-interval D)
(display-interval E)
(display-interval F)

;e2.15
;par2比par1更收敛，但不代表par2更好，两者结果不同，说明区间运算系统有缺陷。

;e2.16
;https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem






