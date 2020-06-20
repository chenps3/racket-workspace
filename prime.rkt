#lang sicp

;判断素数1：n的最小整数因子就是n，时间复杂度O（直到√n）
(define (prime1? n)
  (= (smallest-divisor n) n))

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

;判断素数2，基于费马小定理：如果n是素数，a是小于n的任意正整数，那么a的n次方与a【模n同余】
;【模n同余】除以n的余数相同
;随机取一个a<n,计算a的n次方模n的余数，如果结果不等于a，则n不是素数

;给定检查次数来判断
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;取[1,n-1]之间的随机一个数来检查
;random过程返回比输入小的非负整数
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;计算base的exp次幂对m取模的结果
;模运算分配律  (ab)%m = ((a%m)(b%m))%m
;m为偶数 (a^b)%m = (square (a^(b/2)%m))%m
;m奇数  (a^b)%m = ((a%m)(a^(b-1)%m))%m，当a<m,a%m=a
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (even? a)
  (= (remainder a 2) 0))
















