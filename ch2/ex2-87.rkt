#lang racket

; Problem 2.87
(define (=zero? p)
  (if (empty-termlist? p)
      (the-empty-termlist)
      (and (= (coeff (first-term p)) 0)
           (=zero? (rest-terms p)))))

; Problem 2.88
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (add-poly p2 (negate p2))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

(define (negate L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ([t (first-term L)])
        (adjoin-term (make-term (order t) (negate (coeff t)))
                     (negate (rest-terms L))))))
  
; Problem 2.89
(define (first-term term-list)
  (make-term (- (length term-list) 1) (car term-list)))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons (coeff term) term-list)))