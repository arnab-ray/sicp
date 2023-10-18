#lang racket

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond [(eq? m 'car) x]
          [(eq? m 'cdr) y]
          [(eq? m 'set-car!) set-x!]
          [(eq? m 'set-cdr!) set-y!]
          [else (error "Undefined operation -- CONS" m)]))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (cons item '())])
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue]))


(define (display-enhanced item)
  (display item)
  (display " "))

(define (print-queue queue)
  (define (iter items)
    (cond [(null? items) (newline)]
          [else (display-enhanced (car items))
                (iter (cdr items))]))
  (iter (front-ptr queue)))


(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)