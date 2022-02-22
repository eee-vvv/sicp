#lang sicp

(define (square n) (* n n))

(define (carmichael-test n)
  (carmichael-iter n 1))

(define (carmichael-iter n a)
  (if (< a n)
      (cond ((= (expmod a n n) a) (carmichael-iter n (+ a 1)))
            (else false))
      true))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 2)
      (report-prime (- (runtime)
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes starting-number max primes-found)
  (timed-prime-test starting-number)
  (cond ((> primes-found 2) 0)
        ((even? starting-number) (search-for-primes
                                  (+ 1 starting-number)
                                  max
                                  primes-found))
        (else (if (fast-prime? starting-number 2)
                  (search-for-primes (+ 2 starting-number)
                                     max
                                     (+ 1 primes-found))
                  (search-for-primes (+ 2 starting-number)
                                     max
                                     primes-found)))))

(define (start-search starting-number max)
  (search-for-primes starting-number max 0))
                  
 
