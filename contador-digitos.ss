#lang scheme

(define (contador num)
  (cond
    ((= num 0) 0)
    (else (+ 1 (contador (truncate (/ num 10))))) 
  )
)

(define (contador-digito lista)
  (cond
    ((equal? lista '()) '())
    (else (cons (contador (car lista))
                (contador-digito (cdr lista))))
  )
)


