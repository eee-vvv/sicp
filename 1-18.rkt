#lang sicp

(define (double x) (+ x x))
(define (halve x) (floor (/ x 2)))
(define (even? n) (= (remainder n 2) 0))

(define (mult a b)
  (*-iter a b 0))

(define (*-iter a b n)
  (cond ((= b 0) n)
        ((even? b) (*-iter (double a) (halve b) n))
        (else (*-iter a (- b 1) (+ a n)))))