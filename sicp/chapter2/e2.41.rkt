#lang sicp

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-tuples n)
  (flatmap (lambda (x)
         (map (lambda (y) (cons x y))
              (unique-pairs (- x 1))))
           (enumerate-interval 1 n)))

(define (sum-tuple t)
  (+ (car t) (car (cdr t)) (car (cdr (cdr t)))))

(define (make-tuples n s)
  (filter (lambda (t) (= s (sum-tuple t)))
          (unique-tuples n)))

;test
(unique-tuples 5)
(sum-tuple (list 1 2 3))
(make-tuples 5 10)
