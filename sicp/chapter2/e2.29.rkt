#lang sicp
;(define (make-mobile left right)
;  (list left right))

;(define (make-branch length structure)
;  (list length structure))

;a)
;(define (left-branch mobile)
;  (car mobile))

;(define (right-branch mobile)
;  (car (cdr mobile)))

;(define (branch-length branch)
;  (car branch))

;(define (branch-structure branch)
;  (car (cdr branch)))

;b)
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

(define (mobile? structure)
  (pair? structure))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;c)
(define (mobile-balance? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
  (and (= (moment left) (moment right))
       (branch-balance? left) (branch-balance? right))))

;算力矩
(define (moment branch)
  (* (branch-length branch)
     (branch-weight branch)))
;分支是否平衡
(define (branch-balance? branch)
 (let ((s (branch-structure branch)))
   (if (mobile? s)
       (mobile-balance? s)
       true)))

;d)
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))



;test
(define m1 (make-mobile (make-branch 1 4) (make-branch 2 2)))
m1
(left-branch m1)
(right-branch m1)
(total-weight m1)

(define m2 (make-mobile (make-branch 1 m1) (make-branch 1 m1)))
m2
(left-branch m2)
(right-branch m2)
(total-weight m2)
(mobile-balance? m2)

