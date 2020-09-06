#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;向量的点积
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;矩阵乘以向量
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))
       m))

;矩阵转置
(define (transpose mat)
  (accumulate-n (lambda (a b) (cons a b))
                nil
                mat))

;矩阵乘法
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))
         m)))

(define (m-*-m m n)
   (let ((cols (transpose n)))
     (map (lambda (x) (map (lambda (y) (dot-product x y))
                           cols))
          m)))

;test
(define m (list (list 1 2) (list 3 4)))
(define n (list (list 5 6) (list 7 8)))
(define v (list 5 6))
(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m n)
(m-*-m m n)
