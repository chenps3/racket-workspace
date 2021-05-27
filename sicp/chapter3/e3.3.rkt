#lang sicp

(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "余额不足"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw secret)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "未知操作")))
        (error "密码错误")))
  dispatch)

(define acc (make-account 100 '123456))

((acc '123456 'withdraw) 40)

((acc '321 'deposit) 50)

