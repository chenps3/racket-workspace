#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (head seqs)
  (map (lambda (seq) (car seq)) seqs))

(define (tail seqs)
  (map (lambda (seq) (cdr seq)) seqs))

;test
(define l1 (list 1 2 3))
(define l2 (list 4 5 6))
(define l3 (list 7 8 9))
(define l4 (list 10 11 12))

(accumulate + 0 l1)
(accumulate + 0 l2)
(accumulate + 0 l3)
(accumulate + 0 l4)
(head (list l1 l2 l3 l4))
(tail (list l1 l2 l3 l4))
(accumulate-n + 0 (list l1 l2 l3 l4))