#lang sicp

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

;solution1
(define (reverse l)
  (let ((tail (cdr l)))
    (if (null? tail)
        l
        (append (reverse tail) (list (car l))))))

(define l1 (list 1 2 3 4))

(reverse l1)

;solution 2
(define (reverse2 l)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (car l) acc))))
  (iter l nil))

(re)


