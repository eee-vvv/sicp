#lang sicp

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))
(define (cube x)
  (* (* x x)
     x))

(define (cbrt-iter guess x last)
  (if (good-enough? guess last)
      guess
      (cbrt-iter (improve guess x)
                 x
                 guess)))

(define (improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess last)
  (< (abs (- guess last))
     (/ guess 1000000)))

(define (cbrt x)
  (cbrt-iter 1.0 x 100.0))

(cbrt 27)

(cbrt 0.0000005)