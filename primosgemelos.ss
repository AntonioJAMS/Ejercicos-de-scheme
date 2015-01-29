#lang scheme
(define (primo num)
  (cond
    ((= (veriprimo 1 num) 2) 1)
    (else 0)))

(define (veriprimo i num)
  (cond
    ((> i num) 0)
    ((= (modulo num i) 0) (+ 1 (veriprimo (+ i 1) num)))
    (else (veriprimo (+ i 1) num))))

(define (primos_gemelos limite)
  (primosgemes 1 limite))

(define (primosgemes i num)
  (cond
    ((> i num) '())
    ((and (= (primo i) 1) (= (primo (+ i 2)) 1))
     (cons (cons i (cons (+ i 2) '()))
           (primosgemes (+ i 1) num)))
    (else (primosgemes (+ i 1) num))))