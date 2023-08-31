#lang racket

; Helper
(define (square x) (* x x))

; Problem 3.1
(define (make-accumulator sum)
  (lambda (value)
    (begin (set! sum (+ sum value))
           sum)))

; Problem 3.2
(define (make-monitored f)
  (let ([count 0])
    (define (reset-count)
      (set! count 0)
      count)
    (define (apply-fn arg)
      (begin (set! count (+ count 1))
             (f arg)))
    (define (dispatch m)
      (cond [(eq? m 'how-many-calls?) count]
            [(eq? m 'reset-count) (reset-count)]
            [(number? m) (apply-fn m)]
            [else (error "Unknown request -- MAKE-MONITORED"
                         m)]))
    dispatch))
  
; test
(display "--------Monitored test-----------")
(newline)
(define s (make-monitored square))
(s 10)
(s 3)
(s 'how-many-calls?)
(s 'reset-count)

; Problem 3.3 & 3.4
(define (make-account balance password)
  (let ([count 0]
        [aux-pwd password])
    (define (call-the-cops) "Called cops")
    (define (incorrect-access x)
      (set! count (+ count 1))
       (if (> count 3)
           (call-the-cops)
           "Incorrect password"))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 (set! count 0)
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      (set! count 0)
      balance)
    (define (add-aux-pwd pwd)
      (set! aux-pwd pwd)
      aux-pwd)
    (define (dispatch pwd m)
      (cond [(and (not (eq? pwd password))
                  (not (eq? pwd aux-pwd))) incorrect-access]
            [(eq? m 'withdraw) withdraw]
            [(eq? m 'deposit) deposit]
            [(eq? m 'add-password) add-aux-pwd]
            [else (error "Unknown request -- MAKE-ACCOUNT"
                       m)]))
    dispatch))

; test
(display "--------Account test-----------")
(newline)
(define peter-acc (make-account 100 'secret-password))
((peter-acc 'secret-password 'withdraw) 40)
((peter-acc 'some-other-password 'deposit) 50)
((peter-acc 'secret-password 'add-password) 'bud)
((peter-acc 'bud 'withdraw) 10)
((peter-acc 'some-other-password 'deposit) 50)
((peter-acc 'some-other-password 'deposit) 50)
((peter-acc 'secret-password 'deposit) 20)
((peter-acc 'some-other-password 'deposit) 50)

; Problem 3.5
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (let ([area (* (- x2 x1) (- y2 y1))])
    (* area (monte-carlo trials (lambda () (range-test predicate x1 x2 y1 y2))))))

(define (range-test predicate x1 x2 y1 y2)
  (let ([x (random-in-range x1 x2)]
        [y (random-in-range y1 y2)])
    (predicate x y)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; test
(display "--------Estimate integral-----------")
(newline)
(define P
  (lambda (x y)
    (<= (+ (square (- x 5.0)) (square (- y 7.0))) (square 3.0))))
(define estimated-integral (estimate-integral P 2 8 4 10 425.0))
estimated-integral
(/ estimated-integral (square 3.0))

; Problem 3.6
(define random-init 3)

(define (rand-update x) (+ x 1))

(define rand
  (let ([x random-init])
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-val)
      (set! x new-val))
    (define (dispatch m)
      (cond [(eq? m 'generate) (generate)]
            [(eq? m 'reset) reset]
            [else (error "Unknown request -- RAND"
                         m)]))
    dispatch))

; test
(display "--------Rand test-----------")
(newline)
(rand 'generate)
(rand 'generate)
((rand 'reset) 18)
(rand 'generate)

; Problem 3.7
(define (make-joint acc old-pass new-pass)
  ((acc old-pass 'add-password) new-pass)
  acc)

; test
(display "--------Joint account test-----------")
(newline)
(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud))
((paul-acc 'rosebud 'withdraw) 10)
((peter-acc 'rosebud 'withdraw) 10)
((peter-acc 'secret-password 'withdraw) 10)
  

; Problem 3.8
(define (make-f)
  (let ([prev 1])
    (lambda (x)
      (let ([ret (* prev x)])
        (set! prev x)
        ret))))

(display "--------f test-----------")
(newline)
(define f (make-f))
(+ (f 0) (f 1))
(+ (f 1) (f 0))