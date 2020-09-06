#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enum-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enum-tree (car tree))
                      (enum-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (enum-tree t))))

;test
(define x (cons (list 1 2) (list 3 4)))
(enum-tree x)
(count-leaves x)
