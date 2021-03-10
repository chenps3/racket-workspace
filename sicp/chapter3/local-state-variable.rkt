#lang sicp

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