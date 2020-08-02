#lang sicp
(define x (list (list 1 2) (list 3 4)))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

(define (fringe x)
  (if (null? x)
      x
      (let ((head (car x))
            (tail (cdr x)))
        (if (pair? head)
            (append (fringe head) (fringe tail))
            (append (list head) (fringe tail))))))

(fringe x)
(fringe (list x x))

