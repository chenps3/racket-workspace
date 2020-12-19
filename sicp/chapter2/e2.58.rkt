#lang sicp

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
        (else
         (error "uknown expression type" exp))))

;e2.58 a

(define (=number? e num)
  (and (number? e) (= e num)))

(define (variable? x) (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b) (+ a b)))
        (else (list a '+ b))))

(define (sum? exp)
  (eq? '+ (cadr exp)))

(define (addend exp)
  (car exp))

(define (augend exp)
  (caddr exp))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))

(define (product? exp)
  (eq? '* (cadr exp)))

(define (multipler exp)
  (car exp))

(define (multiplicand exp)
  (caddr exp))

;test
(define xx '(x + (3 * (x + (y + 2)))))
;(sum? xx)
;(addend xx)
;(augend xx)

;(define yy '(x * y))
;(product? yy)
;(multipler yy)
;(multiplicand yy)

(deriv xx 'x)
(deriv '((x * y) * (x + 3)) 'x)

;e2.58 b
(define yy '(x + 3 * (x + y + 2)))
(sum? yy)
(addend yy)
(augend yy)



