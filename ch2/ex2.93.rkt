#lang racket

; Problem 2.93
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (cons n d))
  (define (add-rat x y)
    (make-rat (add (mult (numer x) (denom y))
                   (mult (numer y) (denom x)))
              (mult (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mult (numer x) (denom y))
                   (mult (numer y) (denom x)))
              (mult (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mult (numer x) (numer y))
              (mult (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mult (numer x) (denom y))
              (mult (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))
(define rf (make-rational p2 p1))

(add rf rf)