#lang sicp
(define (cont-frac n d k)
  (define (helper i)
    (/ (n i) (+ (d i)
                (if (= k i)
                    0
                    (helper (+ i 1))))))
  (helper 1))

(define (n i) 1)

(define (d i)
  (if (= (remainder i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))
;test
(d 1)
(d 2)
(d 3)
(d 4)
(d 5)
(d 6)
(d 7)
(d 8)
(d 9)
(d 10)
(d 11)

;k越大，越精确
(define (get-e k)
  (+ 2 (cont-frac n d k)))

