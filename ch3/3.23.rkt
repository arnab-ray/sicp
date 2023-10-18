#lang racket
(require r5rs)

(define (display-enhanced item)
  (display item)
  (display " "))

(define (print-deque queue)
  (define (iter items)
    (cond [(null? items) (newline)]
          [else (display-enhanced (car items))
                (iter (cdr items))]))
  (iter (front-ptr queue)))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))


(define (make-deque) (cons '() '()))

(define (empty-deque? queue)
  (and (null? (front-ptr queue))
       (null? (rear-ptr queue))))

(define (front-deque queue)
  (if (empty-deque? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (rear-deque queue)
  (if (empty-deque? queue)
      (error "REAR called with an empty queue" queue)
      (car (rear-ptr queue))))

(define (front-insert-deque! queue item)
  (let ([new-item (cons item (cons '() '()))])
    (cond [(empty-deque? queue) (set-front-ptr! queue new-item)
                                (set-rear-ptr! queue new-item)]
          [else (set-cdr! (cdr new-item) (front-ptr queue))
                (set-car! (cdr (front-ptr queue)) new-item)
                (set-front-ptr! queue new-item)])))

(define (rear-insert-deque! queue item)
  (let ([new-item (cons item (cons '() '()))])
       (cond [(empty-deque? queue) (set-front-ptr! queue new-item)
                                   (set-rear-ptr! queue new-item)]
             [else (set-car! (cdr new-item) (rear-ptr queue))
                   (set-cdr! (cdr (rear-ptr queue)) new-item)
                   (set-rear-ptr! queue new-item)])))

(define (front-delete-deque! queue)
  (cond [(empty-deque? queue) (error "DELETE! called with an empty queue" queue)]
        [else (set-front-ptr! queue (cddr (front-ptr queue)))
              (set-car! (cdr (front-ptr queue)) '())]))

(define (rear-delete-deque! queue)
  (cond [(empty-deque? queue) (error "DELETE! called with an empty queue" queue)]
        [else (set-rear-ptr! queue (cadr (rear-ptr queue)))
              (set-cdr! (cdr (rear-ptr queue)) '())]))



(define q1 (make-deque))
(front-insert-deque! q1 'a)
(print-deque q1)
(front-insert-deque! q1 'b)
(print-deque q1)
(front-insert-deque! q1 'c)
(print-deque q1)
(front-insert-deque! q1 'd)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(rear-delete-deque! q1)
(print-deque q1)


