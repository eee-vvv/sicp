#lang sicp

(define (square n) (* n n))

(define (even? n) (= (remainder n 2) 0))

(define (fast-expt-r b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-r b (/ n 2))))
        (else (* b (fast-expt-r b (- n 1))))))

(define (fast-expt-i b n)
  (expt-iter 1 b n))

(define (expt-iter a b n)
  (cond ((= n 1) a)
        ((even? n) (expt-iter (* a (square b)) b (/ n 2)))
        (else (expt-iter (* a b) b (- n 1)))))