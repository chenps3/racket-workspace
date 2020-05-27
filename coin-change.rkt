#lang sicp
;树形递归--换零钱

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)  ;amount为0 有1种换零钱方式
        ((or (< amount 0) (= kinds-of-coins 0)) 0) ;amount为负数或者零钱类型为0 有0中换零钱方式
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;取kinds-of-coins种硬币中第一种硬币的面额
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))