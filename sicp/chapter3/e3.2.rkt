#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?) count)
    (define (reset-count) (set! count 0))
    (define (dispatch action)
      (cond ((eq? action 'how-many-calls?) how-many-calls?)
            ((eq? action 'reset-count) reset-count)
            (else (begin (set! count (+ count 1))
                         (f action)))))
    dispatch))

(define (inc i)
  (+ i 1))

(define mf1 (make-monitored inc))

(mf1 1)
(mf1 2)
(mf1 3)
((mf1 'how-many-calls?))
((mf1 'reset-count))
((mf1 'how-many-calls?))
