#lang racket

; Problem 2.85
; add to complex package
(put 'project '(complex)
     (lambda (x) (make-real (real-part x))))

; add to real package
(put 'project '(real)
     (lambda (x)
       (let ([rat (rationalize x)])
         (make-rational (numer x) (denom x)))))

; add to rational package
(put 'project '(rational)
     (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))

(define (drop x)
  (cond [(eq? (type-tag x) 'scheme-number) x]
        [(equ? arg (raise project x)) (drop (project x))]
        [else x]))