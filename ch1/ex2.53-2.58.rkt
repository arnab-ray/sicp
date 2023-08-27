#lang racket

(define (memq item x)
  (cond [(null? x) #f]
        [(eq? item (car x)) x]
        [else (memq item (cdr x))]))

; Problem 2.53
(list 'a 'b 'c)

(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

; Problem 2.54
(define (equal? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(or (null? l1) (null? l2)) #f]
        [else (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))]))

; Problem 2.55
(car ''abracadabra)

; Helper
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list '+ a1 a2)]))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

;(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

(define (multiplier p) (cadr p))

;(define (multiplicand p) (caddr p))

; Problem 2.56
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))]
        [(exponentiation? exp)
         (let ([b (base exp)]
               [e (exponent exp)])
           (if (same-variable? b var)
             (make-product (deriv b var)
                           (make-product e
                                         (make-exponentiation b (make-sum e -1))))
             0))]
        [else
         (error "unknown expression type -- DERIV" exp)]))

(define (make-exponentiation base pow)
  (cond [(=number? pow 0) 1]
        [(=number? pow 1) base]
        [(=number? base 0) 0]
        [(=number? base 1) 1]
        [else (list '** base pow)])) 

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

; Problem 2.57
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand p)
  (accumulate make-product 1 (cddr p)))


; Problem 2.58
(define (make-sum-new a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (addend-new s) (car s))

(define (make-product-new m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (multiplier-new p) (car p))

(define (sum-new? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product-new? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (trimmer s)
  (if (null? (cdr s)) (car s) s))

(define (augend-new s) (trimmer (cddr s)))

(define (multiplicand-new p) (trimmer (cddr p)))
        
(define (deriv-new exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum-new? exp)
         (make-sum-new (deriv-new (addend-new exp) var)
                   (deriv-new (augend-new exp) var))]
        [(product-new? exp)
         (make-sum-new
           (make-product-new (multiplier-new exp)
                         (deriv-new (multiplicand-new exp) var))
           (make-product-new (deriv-new (multiplier-new exp) var)
                         (multiplicand-new exp)))]
        [else
         (error "unknown expression type -- DERIV" exp)]))