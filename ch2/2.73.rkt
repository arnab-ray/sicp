#lang racket

; Helper
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; Problem 2.73
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;; internal procedures
  (define (augend s) (car s))
  (define (addend s) (cdr s))
  (define (make-sum a b) (cons a b))
  (define (deriv-sum s)
    (make-sum (deriv-sum (augend s)) (deriv-sum (addend s))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ s))
  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '(+)
       (lambda (x y) (tag (make-sum x y))))
  'done)

(define (make-sum x y)
  ((get 'make-sum '+) x y))

(define (install-product-package)
  ;; internal procedures
  (define (multiplier p) (car p))
  (define (multiplicand p) (cdr p))
  (define (make-product a b) (cons a b))
  (define (deriv-product p)
    (make-sum
     (make-product (deriv (multiplier p))
                   (multiplicand p))
     (make-product (multiplier p)
                   (deriv (multiplicand p)))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* s))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '(*)
       (lambda (x y) (tag (make-product x y))))
  'done)

(define (make-product x y)
  ((get 'make-product '*) x y))

; Problem 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)]))
  dispatch)