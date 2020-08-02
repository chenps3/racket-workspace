#lang sicp
(define (square x)
  (* x x))

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items nil))

(square-list1 (list 1 2 3 4))

;1 因为cons操作的顺序是从前往后，结果就反过来了

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

(square-list2 (list 1 2 3 4))

;cons第一个操作数总是一个列表而不是单个元素

