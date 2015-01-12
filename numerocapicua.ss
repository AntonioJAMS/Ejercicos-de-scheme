#lang scheme
(define (medir numero)
  (cond
    ((= numero 0) 0)
    (else (+ 1 (medir (truncate (/ numero 10)))))
  )
)

(define (invierte numero)
  (invertir numero (- (medir numero) 1))
)

(define (invertir numero i)
  (cond
    ((= numero 0) 0)
    (else (+ (* (modulo numero 10) (expt 10 i))
             (invertir (truncate (/ numero 10)) (- i 1))))
  )
)

(define (suma numero)
  (+ numero (invierte numero))
)

(define (capicua numero)
  (cond
    ((= (suma numero) (invierte (suma numero))) (suma numero))
    (else (capicua (suma numero)))
  )
)
