#lang racket

; Problem 2.84
(define (raise-to type1 type2)
  (cond [(eq? type1 type2) type1]
        [(get 'raise (list type1)) (raise-to (get 'raise (list type1)) type2)]
        [else #f]))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ([type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)])
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ([u (raise-to type1 type2)]
                          [v (raise-to type2 type1)]
                          [t2->t1 (get-coercion type2 type1)])
                      (cond (u (apply-generic op (u a1) a2))
                            (v (apply-generic op a1 (v a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags))))))))
