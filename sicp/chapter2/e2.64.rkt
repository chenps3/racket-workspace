#lang sicp

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;入参整数n，elts为至少包含n个元素的表
;返回一个包含这个表的前n个元素的平衡树
(define (partial-tree elts n)
  (if (= n 0)
      (cons nil elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (make-tree entry left right)
  (list entry left right))

;a
;partial-tree 的每次调用都会把列表分为两半，且两边都是平衡二叉树，组合起来也是平衡二叉树
;  5
;1   9
; 3 7  11
(list->tree (list 1 3 5 7 9 11))

;b
;O(n)

