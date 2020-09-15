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

; 3/2
(fold-right / 1 (list 1 2 3))

; 1/6
(fold-left / 1 (list 1 2 3))

; (1 (2 (3 ())))
(fold-right list nil (list 1 2 3))

; (((() 1) 2) 3)
(fold-left list nil (list 1 2 3))

;要求性质 a op b = b op a
