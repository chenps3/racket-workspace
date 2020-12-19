#lang sicp

;返回一个过程，过程接收一个向量v，返回一个向量v
;((frame-coord-map a-frame) (make-vect 0 0))
;等价于
;(origin-frame a-frame)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;随便写的
(define (draw-line a b)
  (cons a b))

;e2.46 向量
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v scalar)
  (make-vect (* (xcor-vect v) scalar)
             (* (ycor-vect v) scalar)))

;e2.47 框架
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (car (cdr frame)))

(define (edge2-frame2 frame)
  (cdr (cdr frame)))

;e2.48 线段
(define (make-segment start end)
  (cons start start))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))


;e2.49
;a
(define (outline f)
  (let ((origin (origin-frame f))
        (e1 (edge1-frame f))
        (e2 (edge2-frame f))
        (opposite (add-vect (edge1-frame f) (sub-vect (edge2-frame f) (origin-frame f)))))
    (segments->painter (list (make-segment origin e1)
                             (make-segment origin e2)
                             (make-segment e1 opposite)
                             (make-segment e2 opposite)))))

;b
(define (cross-line f)
  (let ((origin (origin-frame f))
        (e1 (edge1-frame f))
        (e2 (edge2-frame f))
        (opposite (add-vect (edge1-frame f) (sub-vect (edge2-frame f) (origin-frame f)))))
    (segments->painter (list (make-segment origin opposite)
                             (make-segment e1 e2)))))

;c
(define (diamond f)
  (let ((origin (origin-frame f))
        (e1 (edge1-frame f))
        (e2 (edge2-frame f))
        (opposite (add-vect (edge1-frame f) (sub-vect (edge2-frame f) (origin-frame f)))))
    (let ((v1 (scale-vect (add-vect origin e1) 0.5))
          (v2 (scale-vect (add-vect origin e2) 0.5))
          (v3 (scale-vect (add-vect e1 opposite) 0.5))
          (v4 (scale-vect (add-vect e2 opposite) 0.5)))
      (segments->painter (list v1 v2 v3 v4)))))

;d
;

;test
(define v0 (make-vect 0 0))
(define v1 (make-vect 0 1))
(define v2 (make-vect 2 0))

(define f1 (make-frame v0 v1 v2))
(define f2 (make-frame2 v0 v1 v2))

(origin-frame f1)
(edge1-frame f1)
(edge2-frame f1)

(origin-frame2 f2)
(edge1-frame2 f2)
(edge2-frame2 f2)






