#lang racket

; Helper
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; Problem 2.65
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(= (entry set1) (entry set2))
         (make-tree (entry set1)
                    (union-set (left-branch set1) (left-branch set2))
                    (union-set (right-branch set1) (right-branch set2)))]
        [(< (entry set1) (entry set2))
         (make-tree (entry set1)
                    (left-branch set1)
                    (union-set (right-branch set1) set2))]
        [else
         (make-tree (entry set2)
                    (left-branch set2)
                    (union-set set1 (right-branch set2)))]))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(= (entry se1) (entry set2))
         (make-tree (entry set1)
                    (intersection-set (left-branch set1) (left-branch set2))
                    (intersection-set (right-branch set1) (right-branch set2)))]
        [(< (entry set1) (entry set2)) (intersection-set (right-branch set1) set2)]
        [else (intersection-set (left-branch set1) set2)]))

; Problem 2.66
(define (lookup given-key set-of-records)
  (cond [(null? set-of-records) #f]
        [(equal? given-key (entry set-of-records))
         (entry set-of-records)]
        [(< given-key (entry set-of-records))
         (lookup given-key (left-branch set-of-records))]
        [else (lookup given-key (right-branch set-of-records))]))