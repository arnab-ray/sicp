#lang racket


; Problem 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

; Problem 2.45
(define (split f s)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ([smaller (split painter (- n 1))])
          (f painter (s smaller smaller))))))

; Problem 2.46
(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2))
                                    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2))
                                    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; Problem 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (get-origin frame) (car frame))
(define (get-edge1 frame) (cadr frame))
(define (get-edge2 frame) (caddr frame))

(define (make-frame-1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (get-origin-1 frame) (car frame))
(define (get-edge1-1 frame) (cadr frame))
(define (get-edge2-1 frame) (cddr frame))

; Problem 2.48
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; Problem 2.49
(define outline-painter
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
                      (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
                      (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
                      (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0)))))

(define cross-painter
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
                      (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

(define diamond-painter
  (segments->painter (list
                      (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
                      (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
                      (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
                      (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))


