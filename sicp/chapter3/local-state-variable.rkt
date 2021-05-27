#lang sicp

;这个写法不行
(define (new-withdraw amount)
  (let ((balance 100))
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "余额不足")))

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "余额不足"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "余额不足")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 200))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "余额不足"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "未知操作"))))
  dispatch
  )


