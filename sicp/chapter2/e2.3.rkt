#lang sicp
(define (make-rectangle down-left up-right)
  (cons down-left up-right))

(define (down-left rec)
  (car rec))

(define (up-right rec)
  (cdr rec))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;抽象屏障第2层rec-width,rec-height
;如果rec实现变为左上 右下，只需修改rec-width,rec-height
(define (rec-width rec)
  (abs (- (x-point (down-left rec))
          (x-point (up-right rec)))))
(define (rec-height rec)
  (abs (- (y-point (down-left rec))
          (y-point (up-right rec)))))

;抽象屏障第1层rec-perimeter,rec-area
(define (rec-perimeter rec)
  (* 2 (+ (rec-width rec) (rec-height rec))))
(define (rec-area rec)
  (* (rec-width rec) (rec-height rec)))

(define test-rec (make-rectangle
                  (make-point -1 -1)
                  (make-point 6 8)))

(rec-perimeter test-rec)
(rec-area test-rec)