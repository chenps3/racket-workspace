#lang sicp

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (fold-right op init seq)
  (accumulate op init seq))

;e2.39,参考2.18
(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x))
             nil
             sequence))

;test
(define x (list 1 (list 2 3) 4 5))
x
(reverse1 x)
(reverse2 x)