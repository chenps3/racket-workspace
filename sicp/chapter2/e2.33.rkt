#lang sicp

;累加器定义
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;1)
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

;2)
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;3)
(define (length sequence)
  (accumulate (lambda (x y) (+ (if (null? x) 0 1)
                               y))
              0 sequence))


;test
(define seq (list 1 2 3 4 5))
(define (square x) (* x x))
(define seq2 (list 4 5 6 7))

(map square seq)
(append seq seq2)
(length seq1)