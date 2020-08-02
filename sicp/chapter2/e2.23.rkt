#lang sicp
(define (for-each applier items)
  (cond ((not (null? items))
         (applier (car items))
         (for-each applier (cdr items))
      )))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))