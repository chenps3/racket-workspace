#lang sicp

;example
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum (make-product (multipler exp)
;                                 (deriv (multiplicand exp) var))
;                   (make-product (multiplicand exp)
;                                 (deriv (multipler exp) var))))
;        (else
;         (error "uknown expression type" exp))))

(define (variable? e)
  (symbol? e))

(define (same-variable? e1 e2)
  (and (variable? e1) (variable? e2) (eq? e1 e2)))

(define (=number? e num)
  (and (number? e) (= e num)))

;e2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multipler exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                  (deriv (multipler exp) var))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "uknown expression type" exp))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? e)
  (and (pair? e)
       (eq? '** (car e))))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

;e2.57
(define (make-sum . as)
  (define (make-sum-list args)
    (let ((a (car args))
          (b (cadr args)))
      (cond ((> (length args) 2) (make-sum a (make-sum-list (cdr args))))
          ((=number? a 0) b)
          ((=number? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b)))))
  (make-sum-list as))

(define (sum? e)
  (and (pair? e)
       (eq? '+ (car e))))

(define (addend e)
  (cadr e))

(define (augend e)
  (caddr e))

(define (make-product . ms)
  (define (make-product-list args)
    (let ((a (car args))
          (b (cadr args)))
      (cond ((> (length args) 2)
             (make-product a (make-product-list (cdr args))))
            ((or (=number? a 0) (=number? b 0)) 0)
            ((=number? a 1) b)
            ((=number? b 1) a)
            ((and (number? a) (number? b)) (* a b))
            (else (list '* a b)))))
  (make-product-list ms))
        
(define (product? e)
  (and (pair? e)
       (eq? '* (car e))))

(define (multipler e)
  (cadr e))

(define (multiplicand e)
  (caddr e))



;test
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv (make-product 'x 'y '(+ x 3)) 'x)
;(deriv '(** x 5) 'x)

;

