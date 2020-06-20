#lang sicp
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;a能否整除b
(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(define (even? a)
  (= (remainder a 2) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;步数不会减少2倍，因为增加了if判断