#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;返回有理数，分子是n，分母是d
;分子分母同除以最大公约数，可以化为最简形式
(define (make-rat n d)
  (let ((g (gcd n d)))
   (cons (/ n g) (/ d g))))


;返回有理数x的分子
(define (numer x)
  (car x))

;返回有理数x的分母
(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equar-rat? x y)
  (= (* (numer x) (denom y)) 
     (* (numer y) (denom x))))


(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))