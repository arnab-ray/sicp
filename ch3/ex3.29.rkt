#lang racket

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1))
      1
      0))

(define (or-gate a1 a2 output)
  (let ([c1 (make-wire)]
        [c2 (make-wire)]
        [c3 (make-wire)])
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))

