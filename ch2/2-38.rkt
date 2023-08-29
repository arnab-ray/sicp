#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Problem 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Problem 2.37
(define (map-n proc . seqs)
  (if (null? seqs)
      null
      (cons (proc (map car seqs)) (map-n proc (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))

; test
(matrix-*-vector matrix (list 2 3 4 5))

(define (transpose mat)
  (accumulate-n cons null mat))

; test
(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))

; test
(matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))

; Problem 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

; Problem 2.39
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

; test
(reverse-r (list 1 2 3 4))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

; test
(reverse-l (list 1 2 3 4))