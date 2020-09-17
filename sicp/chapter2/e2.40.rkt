#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval begin end)
  (if (> begin end)
      nil
      (cons begin (enumerate-interval (+ begin 1) end))))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

;素数判断
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(define (prime? n)
  (= n (smallest-divisor n)))

;映射+累积
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;给定自然数n，找出所有序对i,j ,满足i+j是素数
(define (prime-sum-pairs n)
  (map make-pair-sum (prime-sum-pair-x n)))

(define (prime-sum-pair-x n)
  (filter prime-sum? (all-pair-x n)))

(define (all-pair-x n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;子序列
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

;test
;(accumulate append nil
;            (map (lambda (i)
;                   (map (lambda (j) (list i j))
;                        (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 5)))
;(enumerate-interval 1 10)
;(prime-sum? (list 1 2))
;(prime-sum-pair-x 10)
(prime-sum-pairs 10)


;e2.40
(define (unique-pairs n)
  (filter (lambda (x) (> (car x) (cadr x))) (all-pair-x n)))

(define (prime-sum-pairs-new n)
  (map make-pair-sum (prime-sum-pair-x n)))
;(unique-pairs 10)

