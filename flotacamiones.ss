#lang scheme
(define (cuenta num)
  (cond
    ((= num 0) 0)
    (else (+ 1 (cuenta (truncate (/ num 10)))))
  )
)

(define (camiones)
  (loscamiones 1 500 0))

(define (loscamiones ini fin c)
  (cond
    ((> ini fin) (cons 'son (cons c (cons 'camiones '()))))
    ((= ini (comprobar 0 ini (expt ini 2))) (cons (cons ini (cons '^2 (cons '= (cons (expt ini 2) '()))))
                                                  (loscamiones (+ ini 1) fin (+ c 1))))
    (else (loscamiones (+ ini 1) fin c))))

(define (comprobar i num num2)
  (cond
    ((= i (cuenta num)) 0)
    (else (+ (* (modulo num2 10) (expt 10 i))
             (comprobar (+ i 1) num (truncate (/ num2 10)))))))
