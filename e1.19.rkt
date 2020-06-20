#lang sicp
;第一次变换
; a-> bq + aq + ap
; b-> bp + aq
;第二次变换
; bq + aq + ap -> (bp+aq)q + (bq+aq+ap)q + (bq + aq + ap)p
; = b(2pq + qq) + a(2pq + qq) + a(pp + qq)
; bp + aq -> (bp+aq)p + (bq + aq + ap)q\
; = b(pp + qq) + a(2pq + qq)
;得到p'= pp + qq, q'=2pq + qq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (square p) (square q))
                                 (+ (* 2 p q) (square q))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (even? a)
  (= (remainder a 2) 0))

(define (square a)
  (* a a))