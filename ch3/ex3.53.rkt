#lang racket
(require racket/stream)

(define ones (stream-cons 1 ones))

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (stream-cons 1 (add-streams ones integers)))

; ex 3.53
(define s (stream-cons 1 (add-streams s s)))

; ex 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (stream-cons 1 (mul-streams integers factorials)))

;ex 3.55
(define (partial-sums s)
  (add-streams s (stream-cons 0 (partial-sums s))))