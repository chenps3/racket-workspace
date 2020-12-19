#lang sicp

(define (key entry)
  (car entry))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;e2.66
(define (entry tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))


(define (lookup1 given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup1 given-key (left set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup1 given-key (right set-of-records)))))
