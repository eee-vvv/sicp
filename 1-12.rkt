#lang sicp

(define (pascal x y)
  (cond ((or (< x 0) (> y x))
         -1)
        ((or (= x 0) (= y 0) (= y x))
         1)
        (else
         (+ (pascal (- x 1) (- y 1))
            (pascal (- x 1) y)))))
