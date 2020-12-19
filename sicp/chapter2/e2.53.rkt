#lang sicp

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)
(car '(a b c))
(cdr '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(x (apple sauce) y apple pear))

;e2.53
(list 'a 'b 'c)
;(a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)
(pair? (car '(a short list)))
;false
(memq 'red '((red shoes) (blue socks)))
;false
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)

;e2.54
(define (equal? list1 list2)
  (cond ((and (null? list1) (not (null? list2))) false)
        ((and (null? list2) (not (null? list1))) false)
        ((and (null? list1) (null? list2)) true)
        ((not (eq? (car list1) (car list2))) false)
        (else (equal? (cdr list1) (cdr list2)))))

(equal? (list 'a 'b 'c) (list 'a 'b))

;e2.55
(car ''abcde)
;等价于
;(car (quote (quote abcde))
; = quote




