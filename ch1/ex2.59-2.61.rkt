#lang racket

; Problem 2.59
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(element-of-set? (car set1) set2) (union-set (cdr set1) set2)]
        [else (cons (car set1) (union-set (cdr set1) set2))]))

; Problem 2.60
(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set) (cons x set))

(define (remove-element x set)
  (cond [(null? set) '()]
        [(equal? x (car set)) (cdr set)]
        [else (cons (car set) (remove-element x (cdr set)))]))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set
                (cdr set1)
                (remove-element (car set1) set2)))]
        [else (intersection-set (cdr set1) set2)]))

(define (union-set-dup set1 set2) (append set1 set2))

; Helper
(define (element-of-set-ordered? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; Problem 2.61
(define (adjoin-set-ordered x set)
  (if (element-of-set-ordered? x set)
      set
      (let ([y (car set)])
       (cond [(null? set) (cons x '())]
            [(< x y) (cons x set)]
            [else (cons y (adjoin-set-ordered x (cdr set)))]))))

(define (union-set-ordered set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(< (car set1) (car set2))
         (cons (car set1) (union-set-ordered (cdr set1) set2))]
        [(> (car set1) (car set2))
         (cons (car set2) (union-set-ordered set1 (cdr set2)))]
        [else
         (cons (car set1) (union-set-ordered (cdr set1) (cdr set2)))]))
  