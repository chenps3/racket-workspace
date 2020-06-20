#lang sicp
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

;(gcd 206 40)
;(if (= 40 0) 206 (gcd 40 (remainder 206 40)))

;(gcd 40 (remainder 206 40))
;(if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))  +1

;(gcd (remainder 206 40)
;     (remainder 40 (remainder 206 40)))
;(if (= (remainder 40 (remainder 206 40) 0))                                       +2
;       (remainder 206 40)
;       (gcd (remainder 40 (remainder 206 40))
;            (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

;(gcd (remainder 40 (remainder 206 40))
;            (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)        +4
;    (remainder 40 (remainder 206 40))
;    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;         (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) 

;(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;     )
;(if (= (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) +7
;    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;    (gcd (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;         (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;                    (remainder (remainder 40 (remainder 206 40))  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

;(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) + 4

;正则序1 + 2 + 4 + 7 + 4 = 18

;(gcd 206 40)
;(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
;(gcd 40 6)       +1
;(if (= 6 0) 40 (gcd 6 (remainder 40 6)))
;(gcd 6 4)        +1
;(if (= 4 0) 6 (gcd 4 (remainder 6 4)))
;(gcd 4 2)        +1
;(if (= 2 0) 4 (gcd 2 (remainder 4 2)))
;(gcd 2 0)        +1
;(if (= 0 0) 2 (gcd 0 (remainder 2 0)))
;2
;应用序 1+1+1+1 = 4