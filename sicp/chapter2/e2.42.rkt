#lang sicp

;(define (queen board-size)
;  (define (queen-cols k)
;    (if (= k 0)
;        (list empty-board)
;        (filter
;         (lambda (positions) (safe? k positions))
;         (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens))
;                 (enumerate-interval 1 board-size)))
;          (queen-cols (- k 1))))))
;  (queen-cols board-size))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      nil
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define empty-board nil)

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (adjoin-position new-row k rest-of-queens)
  (append (list (list new-row k)) rest-of-queens))

;如果在同一条斜线,只有两种可能,一是横纵坐标之和相等,一是横纵坐标之差相等，即斜率为+-1
(define (safe-queen? q1 q2)
  (let ((row1 (car q1))
        (col1 (cadr q1))
        (row2 (car q2))
        (col2 (cadr q2)))
   (and (not (= row1 row2))
        (not (= (- row1 col1) (- row2 col2)))
        (not (= (+ row1 col1) (+ row2 col2))))))

;test
;(enumerate-interval 1 10)
;(accumulate + 0 (list 1 2 3 4 5))
;(adjoin-position 1 5 (list (list 1 3) (list 1 4)))
