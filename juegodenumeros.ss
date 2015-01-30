#lang scheme
(define (numeros num)
  (losnumeros 0 num))

(define (losnumeros i num)
  (cond
    ((= i num) 0)
    (else (+ (* 1 (expt 10 i))
             (losnumeros (+ i 1) num)))))

(define (listadenumero limite)
  (listanum 1 limite))

(define (listanum i limite)
  (cond
    ((> i limite) '())
    (else (cons (cons 'N 
                      (cons '= 
                            (cons i
                                  (cons '->
                                        (cons (numeros i)
                                              (cons '* 
                                                    (cons (numeros i) 
                                                          (cons '=
                                                                (cons (* (numeros i) (numeros i)) '())))))))))
                (listanum (+ i 1) limite)))))
                