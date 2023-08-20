#lang racket

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (square a) (* a a))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Problem 1.29
(define (cube a) (* a a a))

(define (even? a) (= (remainder a 2)))

(define (integral f a b n)  
  (let ([h (/ (- b a) n)])
    (define (integral-term a)
      (+ (f a) (* 4 (f (+ a h))) (f (+ a (* 2 h)))))
    (define (next-term a)
      (+ a (* 2 h)))
    (* (/ h 3.0) (sum integral-term a next-term (- b (* 2 h))))))

; Problem 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Problem 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (identity a) a)
  (define (inc a) (+ a 1))
  (product identity 1 inc n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-product a b)
  (define (pi-term a) (* (/ (+ a 1.0) (+ a 2.0)) (/ (+ a 3.0) (+ a 2.0))))
  (define (pi-next a) (+ a 2))
  (product-iter pi-term a pi-next b))

; Problem 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Problem 1.33
(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (if (filter a) (term a) null-value)
       (filtered-accumulate combiner filter null-value term (next a) next b))))

(define (filtered-accumulate-iter combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner
                        (if (filter a) (term a) null-value)
                        result))))
  (iter a null-value))

(define (square-sum-primes a b)
  (define (inc a) (+ a 1))
  (filtered-accumulate-iter + prime? 0 square a inc b))

; Problem 1.34
(define (f g)
  (g 2))

; Problem 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (print-guess guess)
    (display guess)
      (newline))
  (define (try guess)
    (let ([next (f guess)])
      (print-guess guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; Problem 1.36
(define (exp-x x)
  (fixed-point (lambda (y) (/ (log 1000) (log y))) 10.0))

(define (average x y)
  (/ (+ x y) 2))

(define (exp-x-damped x)
  (fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 10.0))

; Problem 1.37
(define (cont-frac-iter n d k)
  (define (term count)
    (if (= count k)
        (/ (n k) (d k))
        (/ (n k) (+ (d k) (term (+ count 1))))))
  (term 1))

(define (cont-frac n d k)
  (if (= k 1)
      1
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

; Problem 1.38
(define (euler-approx k)
  (+ 2.0
     (cont-frac
      (lambda (i) 1.0)
      (lambda (i)
        (if (= (remainder i 3) 2)
         (/ (+ i 1) 1.5)
         1))
      k)))

; Problem 1.39
(define (tan-cf x k) 
  (cont-frac
   (lambda (i) (if (= i 1) i (-(* i i))))
   (lambda (i) (- (* 2 i) 1))
   k))









  