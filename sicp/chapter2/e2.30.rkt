#lang sicp

;example
(define (scale-tree1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))

(scale-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

;e2.30
(define (square x)
  (* x x))

;1)不使用map实现
(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(square-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;2)使用map
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))
(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

