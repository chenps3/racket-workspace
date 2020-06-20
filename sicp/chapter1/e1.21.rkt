#lang sicp
;找出n的最小整数因子（大于1）
(define (smallest-divisor n)
  (find-divisor n 2))

;从2开始一个个试，看看是否能整除n，直到√n
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;b是否能被a整除
(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(smallest-divisor 199)
;(find-divisor 199 2)  ->
;(find-divisor 199 3)  ->
;...
;(find-divisor 199 15)  ->
;(cond ((> (square 15) 199) 199)
;      ((divides? 15 199) 15)
;      (else (find-divisor 199 (+ 15 1)))) ->
;199

(smallest-divisor 1999)
;(find-divisor 1999 2)  ->
;(find-divisor 1999 3)  ->
;...
;(find-divisor 1999 45)  ->
;(cond ((> (square 45) 1999) 1999)
;      ((divides? 45 1999) 45)
;      (else (find-divisor 1999 (+ 45 1)))) ->
;1999

(smallest-divisor 19999)
;(find-divisor 19999 2)  ->
;(find-divisor 19999 3)  ->
;...
;(find-divisor 19999 7)  ->
;(cond ((> (square 7) 19999) 19999)
;      ((divides? 7 19999) 7)
;      (else (find-divisor 19999 (+ 7 1)))) ->
;7