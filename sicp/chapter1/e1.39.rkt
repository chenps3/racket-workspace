#lang sicp

(define (square a)
  (* a a))

(define (cont-frac n d k)
  (define (helper i)
    (/ (n i) (+ (d i)
                (if (= k i)
                    0
                    (helper (+ i 1))))))
  (helper 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i) (- (* i 2) 1))
  (cont-frac n d k))