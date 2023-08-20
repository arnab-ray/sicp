#lang racket

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
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

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))

(define (square x) (* x x))

; Problem 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; Problem 1.41
(define (inc a) (+ a 1))

(define (double f)
  (lambda (x) (f (f x))))

; Problem 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; Problem 1.43
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

; Problem 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Problem 1.45
(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

 (define (fast-expt b n) 
   (cond ((= n 0) 1) 
         ((even? n) (square (fast-expt b (/ n 2)))) 
         (else (* b (fast-expt b (- n 1))))))

(define (nth-root x n f) 
   (fixed-point ((repeated average-damp f)  
                 (lambda (y) (/ x (fast-expt y (- n 1)))))
                1.0))

; Problem 1.46
(define (iterative-improve close-enough? improve)
  (lambda (guess)
    (if (close-enough? guess)
        guess
        ((iterative-improve close-enough? improve) (improve guess)))))

(define (fixed-point-generic f first-guess)
  ((iterative-improve
    (lambda (x) (close-enough? x (f x)))
    f)
   first-guess))

(define (sqrt-generic x)
  ((iterative-improve
    (lambda (y) (< (abs (- (square y) x)) 0.001))
    (lambda (y) (average x (/ x y))))
   1.0))