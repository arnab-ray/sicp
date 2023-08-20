#lang racket

; Problem 1.12
(define (pascal m n)
  (cond [(= n 0) 1]
        [(= (+ m 1) n) 1]
        [else (+ (pascal (- m 1) (- n 1)) (pascal (- m 1) n))]))

; Problem 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(define (fast-expt-iter b counter product)
  (cond [(= counter 0) product]
        [(even? counter) (fast-expt-iter (square b) (/ counter 2) product)]
        [else (fast-expt-iter b (- counter 1) (* b product))]))

; Problem 1.17
(define (double n) (+ n n))

(define (halves n) (/ n 2))

(define (fast-mult a b)
  (cond [(= b 0) 0]
        [(even? b) (fast-mult (double a) (halves b))]
        [else (+ a (fast-mult a (- b 1)))]))

; Problem 1.18
(define (fast-mult-iter a b product)
  (cond [(= b 0) product]
        [(even? b) (fast-mult-iter (double a) (halves b) product)]
        [else (fast-mult-iter a (- b 1) (+ a product))]))

; Problem 1.19
(define (fib n) 
   (fib-iter 1 0 0 1 n)) 
 (define (fib-iter a b p q count) 
   (cond [(= count 0) b] 
         [(even? count)
          (fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ count 2))] 
         [else
          (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1))]))

; Problem 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

; Problem 1.22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes low high count)
  (cond [(> low high) (display "")]
        [(= count 0) (display "")]
        [(even? low) (search-for-primes (+ low 1) high count)]
        [(timed-prime-test low) (search-for-primes (+ low 2) high (- count 1))]
        [else (search-for-primes (+ low 2) high count)]))

; Problem 1.23
(define (smallest-divisor-fast n)
  (find-divisor-next n 2))

(define (find-divisor-next n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor-next n (next test-divisor))]))

(define (next n)
  (if (= n 2) (+ n 1) (+ n 2)))

; Problem 1.24
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp) (remainder (square (expmod base (/ exp 2) m)) m)]
        [else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test-fast n)
  (start-prime-test-fast n (current-inexact-milliseconds)))

(define (start-prime-test-fast n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      #f))

(define (search-for-primes-fast low high count)
  (cond [(> low high) (display "")]
        [(= count 0) (display "")]
        [(even? low) (search-for-primes-fast (+ low 1) high count)]
        [(timed-prime-test-fast low) (search-for-primes-fast (+ low 2) high (- count 1))]
        [else (search-for-primes-fast (+ low 2) high count)]))


; Problem 1.28
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (expmod base exp m)
    (cond [(= exp 0) 1]
          [(even? exp) (squaremod (expmod base (/ exp 2) m) m)]
          [else (remainder (* base (expmod base (- exp 1) m)) m)]))
  (define (squaremod a m)
    (let ([rem (remainder (square a) m)])
      (cond [(and (= rem 1) (not (= a 1)) (not (= a (- n 1)))) 0]
            [else rem])))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-miller? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-miller? n (- times 1)))
        (else false)))