#lang racket
(require racket/stream)

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(stream->list (stream-take (expand 1 7 10) 10))


; ex 3.59

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define cosine-series
  (stream-cons 1 (integrate-series (scale-stream sine-series -1))))

; ex 3.60
(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (scale-stream (stream-rest s2) (stream-first s1))
                            (mul-series (stream-rest s1) s2))))

; ex 3.61
(define (invert-unit-series s)
  (stream-cons 1 (scale-stream (mul-series (stream-rest s) (invert-unit-series s)) -1)))

; ex 3.62
(define (div-series nums dens)
  (mul-series nums (invert-unit-series dens)))

(define tangent-series (div-series sine-series cosine-series))