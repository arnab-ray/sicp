#lang racket

(define (abs n) (if (< n 0) (- 0 n) n))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Problem 2.1

(define (make-rat n d)
  (cond [(or (< d 0) (and (< n 0) (< d 0))) (cons (abs n) (abs d))]
        [else (cons n d)]))

; Helper method

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
  (/ (+ x y) 2.0))

; Problem 2.2

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define seg (make-segment (make-point 2 3) (make-point 10 15)))

(print-point (midpoint-segment seg))

; Problem 2.3
(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))

(define (width-rectangle r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (height-rectangle r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))
  
(define (perimeter-rectangle r)
  (* 2.0 (+ (width-rectangle r) (height-rectangle r))))
  
(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

; Problem 2.4
(define (cons-mod x y)
  (lambda (m) (m x y)))

(define (car-mod z)
  (z (lambda (p q) p)))

(define (cdr-mod z)
  (z (lambda (p q) q)))

; Problem 2.5
(define (exp base n)
  (define (iter count result)
    (if (= count n)
        result
        (iter (+ count 1) (* result base))))
  (iter 1 base))

(define (count-divisibility n divisor)
  (define (iter pow)
    (if (= 0 (remainder n (exp divisor pow))) (+ pow 1) (- pow 1)))
  (iter 1))

(define (cons-new-mod a b)
  (* (exp 2 a) (exp 3 b)))

(define (car-new-mod x) (count-divisibility x 2))

(define (cdr-new-mod x) (count-divisibility x 3))

; Problem 2.6
(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) (a f) ((b f) x))))

; Problem 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))


;Helper
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
; Problem 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Problem 2.10
(define (div-interval x y)
  (if (<= 0 (* (lower-bound y) (upper-bound y)))
  (error "Interval spans zero")
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

; Problem 2.12
(define (make-center-percent c p)
  (let ([w (* c (/ p 100.0))])
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let ([c (/ (+ (lower-bound i) (upper-bound i)) 2.0)]
        [w (/ (- (upper-bound i) (lower-bound i)) 2.0)])
    (* (/ w c) 100)))

; Problem 2.17
(define (last-pair l)
  (cond [(null? l) null]
        [(null? (cdr l)) (car l)]
        [else (last-pair (cdr l))]))


; Problem 2.18
(define (reverse l)
  (cond [(null? l) null]
        [(null? (cdr l)) l]
        [else (append (reverse (cdr l)) (list (car l)))]))

; Problem 2.19
(define (first-denomination l) (car l))

(define (except-first-denomination l) (cdr l))

(define (no-more? l) (null? l))

; Problem 2.20
(define (same-parity f . r)
  (define (iter l elements target-remainder)
    (if (null? l)
        elements
        (iter (cdr l)
              (if (= (remainder (car l) 2) target-remainder)
                  (append elements (list (car l)))
                  elements)
              target-remainder)))
  (iter r (list f) (remainder f 2)))

; Problem 2.21
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list (cdr items)))))

; Helper
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

; Problem 2.23
(define (for-each proc items)
  (cond [(null? items) (newline)]
        [else (proc (car items)) (for-each proc (cdr items))]))

; Problem 2.26


; Problem 2.27
(define (deep-reverse l)
  (cond [(null? l) null]
        [(not (pair? l)) (car l)]
        [else (append (deep-reverse (cdr l)) (list (reverse (car l))))]))

; Problem 2.28
(define (fringe t)
  (cond [(null? t) null]
        [(not (pair? t)) (list t)]
        [else (append (fringe (car t)) (fringe (cdr t)))]))

; Problem 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length b) (car b))

(define (branch-structure b) (car (cdr b)))

(define (branch-weight b)
  (cond [(null? b) 0]
        [(not (pair? (branch-structure b))) b]
        [else (+
               (branch-weight (left-branch (branch-structure b)))
               (branch-weight (right-branch (branch-structure b))))]))
  
(define (total-weight mobile)
  (cond [(null? mobile) 0]
        [(not (pair? mobile)) mobile]
        [else (+
               (total-weight (branch-structure (left-branch mobile)))
               (total-weight (branch-structure (right-branch mobile))))]))

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))
  
(define (is-balanced? mobile)
  (cond [(null? mobile) #t]
        [(not (pair? mobile)) #t]
        [else (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
                   (is-balanced? (branch-structure (left-branch mobile)))
                   (is-balanced? (branch-structure (right-branch mobile))))]))

; Helper
(define (square a) (* a a))

; Problemm 2.30
(define (square-tree tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (square tree)]
        [else (cons (square-tree (car tree)) (square-tree (cdr tree)))]))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

; Problem 2.31
(define (tree-map proc tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (proc tree)]
        [else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree)))]))

(define (square-tree-abs tree) (tree-map square tree))

; Problem 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))