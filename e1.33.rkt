#lang sicp
;迭代版本
(define (filterer-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

;素数判断，最小整数因子是不是自己
(define (prime? a)
  (= (smallest-divisor a) a))

;寻找最小整数因子，从2开始检查是否能整除a
(define (smallest-divisor a)
  (find-divisor a 2))

;判断test-divisor是否能被n整除，是的话返回test-diviso，否则加1继续直到n开方
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((= (remainder n test-divisor) 0) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (square a)
  (* a a))

;与n互素判断
(define (n-prime? i n)
  (= (gcd i n) 1))

(define (gcd a b)
  (cond ((< a b) (gcd b a))
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (identity x) x)
(define (next x) (+ x 1))

;测试，求a到b的素数之和
(define (sum-prime a b)
  (filterer-accumulate + 0 identity a next b prime?))

;测试，小于n且与n互素的正整数的乘积
(define (product-n-prime n)
  (define (filter a)
    (n-prime? a n))
  (filterer-accumulate * 1 identity 1 next n filter))