#lang racket

;Helper Huffman coding
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;Problem 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; Problem 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-sym symbol curr-bits current-branch)
  (if (null? current-branch)
      '()
      (if (leaf? current-branch)
          (if (equal? (symbol-leaf current-branch) symbol)
              curr-bits
              '())
          (let ([left (encode-sym symbol (append curr-bits (list 0)) (left-branch current-branch))]
                [right (encode-sym symbol (append curr-bits (list 1)) (right-branch current-branch))])
            (cond [(and (null? left) (null? right)) '()]
                  [(null? left) right]
                  [else left])))))

(define (encode-symbol symbol tree)
  (let ([left (encode-sym symbol (list 0) (left-branch tree))]
        [right (encode-sym symbol (list 1) (right-branch tree))])
    (cond [(and (null? left) (null? right))
           (error "bad symbol" symbol)]
          [(null? left) right]
          [else left])))

(encode '(A D A B B C A) sample-tree)

; Problem 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (sort pairs
            (lambda (x y) (< (cadr x) (cadr y))))))

(define (successive-merge pairs)    
  (define (merge-helper tree pairs)
    (if (null? pairs)
        tree
        (let ([x (car pairs)])
          (if (null? tree)
              (if (not (null? (cdr pairs)))
                  (let ([y (cadr pairs)])
                    (merge-helper
                     (make-code-tree (make-leaf (car x) (cadr x))
                                     (make-leaf (car y) (cadr y)))
                     (cddr pairs)))
                  x)
              (merge-helper
               (make-code-tree (make-leaf (car x) (cadr x)) tree)
               (cdr pairs))))))
  (merge-helper '() pairs))

; Problem 2.70
(define lyrics 
   '((A 2) 
     (BOOM 1) 
     (GET 2) 
     (JOB 2) 
     (NA 16) 
     (SHA 3) 
     (YIP 9) 
     (WAH 1)))

(generate-huffman-tree '((A 2) (B 1)))

(define lyrics-tree (generate-huffman-tree lyrics))

(define song 
   '(GET A JOB 
     SHA NA NA NA NA NA NA NA NA 
     GET A JOB 
     SHA NA NA NA NA NA NA NA NA 
     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
     SHA BOOM))

(display "huffman-encoding length: ")
(display (length (encode song lyrics-tree)))
(newline)

(display "min fixed-length encoding length: ")
(display (* (log (length lyrics) 2) (length song)))
(newline) 