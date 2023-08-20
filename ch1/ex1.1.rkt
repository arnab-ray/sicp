#lang racket

(/ (+ 5 4 (- 2 (- 3 (+ (/ 4 5) 6))))
   (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-larger-two x y z)
  (cond [(and (< x y) (< x z)) (sum-of-squares y z)]
        [(and (< y x) (< y z)) (sum-of-squares x z)]
        [else (sum-of-squares x y)]))