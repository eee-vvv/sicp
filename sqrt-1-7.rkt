#lang sicp

(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define(sqrt-iter guess x last)
  (if (good-enough? guess last)
      guess
      (sqrt-iter (improve guess x)
                 x
                 guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess last)
  (< (abs (- guess last))
     (/ guess 1000000)))

(define (sqrt x)
  (sqrt-iter 1.0 x 100.0))

(sqrt (* 9876543210123456789
         9123456789876543212))

(sqrt 9)

(sqrt 0.0000005)
