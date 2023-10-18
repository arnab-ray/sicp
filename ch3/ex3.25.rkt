#lang racket
(require r5rs)

(define (make-table)
    (define table '())
    (define (lookup keys)
	(let ((record (assoc keys table)))
	    (and record (cadr record))))
    (define (insert keys value)
	(let ([record (assoc keys table)])
	    (cond [record  (set-car! (cdr record) value)]
		  [else    (set! table (cons (list keys value) table))])))
    (define (dispatch m)
        (cond [(eq? m 'lookup-proc) lookup]
	      [(eq? m 'insert-proc!) insert]
	      [else (error "Unknown request -- NTABLE" m)]))
    dispatch)


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(a b c) 17) 
(get '(a b c))
(get '(a c))
(get '(a b))