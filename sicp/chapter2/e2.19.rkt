#lang sicp

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (expect-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (expect-first-denomination coin-values)
  (cdr coin-values))

;不影响


(define us-coins (list 50 25 10 5 1))
(define us-coins2 (list 50 5 25 10 1))


(cc 100 us-coins)
(cc 100 us-coins2)