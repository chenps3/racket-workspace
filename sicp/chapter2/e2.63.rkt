#lang sicp

(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left set)))
        ((> x (entry set)) (element-of-set? x (right set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left set))
                                      (right set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left set)
                                      (adjoin-set x (right set))))))
;e2.63
(define (tree->list-1 tree)
  (if (null? tree)
      nil
      (append (tree->list-1 (left tree))
              (cons (entry tree)
                    (tree->list-1 (right tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left tree)
                      (cons (entry tree)
                            (copy-to-list (right tree) result-list)))))
  (copy-to-list tree nil))

(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil)) (make-tree 9 nil (make-tree 11 nil nil))))

(define tree2 (make-tree 3 (make-tree 1 nil nil) (make-tree 7 (make-tree 5 nil nil) (make-tree 9 nil (make-tree 11 nil nil)))))

(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 nil nil) nil) (make-tree 9 (make-tree 7 nil nil) (make-tree 11 nil nil))))

(tree->list-1 tree1)
(tree->list-1 tree2)
(tree->list-1 tree3)

(tree->list-2 tree1)
(tree->list-2 tree2)
(tree->list-2 tree3)

;a 相同
;b 法1 O(nlogn)  append的时间复杂度为n
;法2 O(logn)