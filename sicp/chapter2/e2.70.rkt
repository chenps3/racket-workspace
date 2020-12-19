#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? x)
  (eq? 'leaf (car x)))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (succesive-merge (make-leaf-set pairs)))

(define (succesive-merge leaf-set)
  (cond ((null? leaf-set) nil)
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (succesive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                                           (cddr leaf-set))))))

(define test-pair (list (list 'A 2)
                        (list 'NA 16)
                        (list 'BOOM 1)
                        (list 'SHA 3)
                        (list 'GET 2)
                        (list 'YIP 9)
                        (list 'JOB 2)
                        (list 'WAH 1)))

(define test-tree (generate-huffman-tree test-pair))

(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol m tree)
  (if (memq m (symbols tree))
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (memq m (symbols left))
            (if (leaf? left)
                (list 0)
                (cons 0 (encode-symbol m left)))
            (if (leaf? right)
                (list 1)
                (cons 1 (encode-symbol m right)))))
      (error "找不到符号")))

(encode '(GET A JOB) test-tree)
(encode '(SHA NA NA NA NA NA NA NA NA) test-tree)
(encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) test-tree)
(encode '(SHA BOOM) test-tree)



