#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;get 未实现
(define (get op type) nil)

;put 未实现
(define (put op type item) nil)

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;a
;number? 和 same-variable? 参数都是基本类型

;b
(define (make-sum a b) (list '+ a b))

(define (make-product a b) (list '* a b))

(define (deriv-sum operands var)
  (make-sum (deriv (car operands) var)
            (deriv (cadr operands) var)))

(define (deriv-product operands var)
  (make-sum
   (make-product (car operands)
                 (deriv (cadr operands) var))
   (make-product (deriv (car exp) var)
                 (cadr exp))))

(put 'deriv '+ deriv-sum)

(put 'deriv '* deriv-product)

;c
;略

;d
;put方法变一下参数顺序


















