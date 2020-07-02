#lang sicp
;;将f应用于n 0次
(define zero (lambda (f) (lambda (x) x)))

;将f再应用于n 1次
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;((lambda (f) (lambda (x) (f ((zero f) x)))))
;((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
;((lambda (f) (lambda (x) (f x))))

(define one (lambda (f) (lambda (x) (f x))))

;(add-1 one)
;((lambda (f) (lambda (x) (f ((one f) x)))))
;((lambda (f) (lambda (x) (f ((lambda (x) (f x)) x)))))
;((lambda (f) (lambda (x) (f (f x)))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;这个语言中，数n表示函数f对x应用了n次
;(one f) => 返回一个函数，接受参数x，对x应用1次
;(two f) => 返回一个函数，接受参数x，对x应用2次
;以此类推
;(a f) => 返回一个函数，接受参数x，对x应用a次
;由此得到自定义的加法,先对x应用b次f，再应用a次f，反过来也行
(define (plus a b)
   (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;测试，令x=0，f(x) = x+1

(define (inc x) (+ x 1))

((zero inc) 0)
((one inc) 0)
((two inc) 0)





