#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? 'leaf (car object)))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;结点加入到有序集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;pair列表转为有序的叶节点列表
(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;e2.69
(define (succesive-merge tree-set)
  (cond ((null? tree-set) nil)
        ((null? (cdr tree-set)) (car tree-set))
        (else (succesive-merge (adjoin-set (make-code-tree (car tree-set) (cadr tree-set))
                                           (cddr tree-set))))))

;构造huffman树
(define (generate-huffman-tree pairs)
  (succesive-merge (make-leaf-set pairs)))

;编码
(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (memq symbol (symbols tree))
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (memq symbol (symbols left))
            ;symbol在左边
            (if (leaf? left)
                (list 0)
                (cons 0 (encode-symbol symbol left)))
            ;symbol在右边
            (if (leaf? right)
                (list 1)
                (cons 1 (encode-symbol symbol right)))))
      (error "找不到符号")))

;解码
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE BRANCH" bit))))

;test
(define a (list 'a 4))
(define b (list 'b 2))
(define c (list 'c 1))
(define d (list 'd 1))
(define test-pairs (list a b c d))

(define test-tree (generate-huffman-tree test-pairs))
(define test-msg '(a c d b a))
(define test-code (encode test-msg test-tree))
(decode test-code test-tree)

;



