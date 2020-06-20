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
        (else (find-divisor n (+ 1 test-divisor)))))

;a能否整除b
(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(define (even? a)
  (= (remainder a 2) 0))

(define (search-for-primes start end)
  (search-for-primes-iter
   (if (even? start) (+ start 1) start)
   end))

(define (search-for-primes-iter n end)
  (cond ((<= n end) (timed-prime-test n) (search-for-primes-iter (+ n 2) end))))

