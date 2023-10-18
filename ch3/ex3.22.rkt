#lang racket

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car (front-ptr))))

    (define (insert-queue! item)
      (let ([new-pair (mcons item '())])
        (cond [(empty-queue?) (set-front-ptr! new-pair)
                              (set-rear-ptr! new-pair)]
              [else (set-mcdr! rear-ptr new-pair)
                    (set-rear-ptr! new-pair)])
        front-ptr))

    (define (delete-queue!)
      (cond [(empty-queue?) (error "DELETE! called with an empty queue")]
            [else (set-front-ptr! (mcdr front-ptr))])
      front-ptr)
    
    (define (dispatch m)
      (cond [(eq? m 'empty-queue?) (empty-queue?)]
            [(eq? m 'front-queue) (front-queue)]
            [(eq? m 'insert-queue!) insert-queue!]
            [(eq? m 'delete-queue!) (delete-queue!)]
            [else (error "Undefined operation -- CONS" m)]))
    dispatch))


(define q1 (make-queue))
(q1 'empty-queue?)
((q1 'insert-queue!) 1)
(q1 'delete-queue!)
((q1 'insert-queue!) 1)
((q1 'insert-queue!) 2)
((q1 'insert-queue!) 3)
(q1 'delete-queue!)