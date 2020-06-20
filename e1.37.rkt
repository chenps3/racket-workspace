#lang sicp

(define (close-enough? a b) 
  (< (abs (- a b)) 0.0001))

;recursive
(define (cont-frac n d k)
  (define (helper i)
    (/ (n i) (+ (d i)
                (if (= k i)
                    0
                    (helper (+ i 1))))))
  (helper 1))

;iter
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

  
(define (gold-guess k) (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))
(define gold 1.6180)
;k=10
(define (get-k k)
  (if (close-enough? (gold-guess k) (/ 1 gold))
      k
      (get-k (+ k 1))))


