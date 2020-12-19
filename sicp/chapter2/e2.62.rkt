#lang sicp

;时间复杂度n2
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        (union-set (cdr set1) (adjoin-set (car set1) set2))))

;时间复杂度n
;遍历set1 set2 加入到新集合
(define (union-set set1 set2)
  (define (union-set-iter s1 s2 acc)
    (let ((x1 (car s1))
          (x2 (car s2)))
      (cond ((null? s1) s2)
            ((null? s2) s1)
            ((< x1 x2) (union-set-iter (cdr s1) s2 (append acc (list x1))))
            ((> x1 x2) (union-set-iter s1 (cdr s2) (append acc (list x2))))
            (else (union-set-iter (cdr s1) (cdr s2) (append acc (list x1)))))))
  (union-set-iter set1 set2 nil))
