#lang scheme
(define (elimina num dig)
  (elimina-elimina num dig 0) 
)

(define (elimina-elimina num dig i)
  (cond
    ((= num 0) 0)
    ((= (modulo num 10) dig) (elimina-elimina (truncate (/ num 10)) dig i))
    (else (+ (* (modulo num 10) (expt 10 i))
             (elimina-elimina (truncate (/ num 10)) dig (+ i 1))))
  )
)

(define (elimina-digito lista dig)
  (cond
    ((equal? lista '()) '())
    ((= (elimina (car lista) dig) 0) '())
    (else (cons (elimina (car lista) dig)
                (elimina-digito (cdr lista) dig)))
  )
)




