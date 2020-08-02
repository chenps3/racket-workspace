#lang sicp
(define (reverse items)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (car l) acc))))
  (iter items nil))

(reverse (list 1 4 9 16 25))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

(define (deep-reverse x)
  (define (iter l acc)
    (if (null? l)
        acc
        (let ((head (car l))
              (tail (cdr l)))
          (if (pair? head)
              (iter tail (append (list (deep-reverse head)) acc))
              (iter tail (append (list head) acc))))))
  (iter x nil))


(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)
