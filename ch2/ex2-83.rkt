#lang racket

; Problem 2.83
(define (raise x) (apply-generic 'raise x))

; add to integer package
(put 'raise '(integer)
     (lambda (x) (make-rational x 1)))

; add to rational package
(put 'raise '(rational)
     (lambda (x) (make-real (* 1.0 (/ (numer x) (denom x))))))

; add to real package
(put 'raise '(real)
     (lambda (x) (make-complex-from-real-imag x 0)))
