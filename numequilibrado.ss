#lang scheme
(define (cuenta num)
  (cond
    ((= num 0) 0)
    (else (+ 1 (cuenta (truncate (/ num 10)))))))

(define (voltea num)
  (volteando num (- (cuenta num) 1)))

(define (volteando num i)
  (cond
    ((= num 0) 0)
    (else (+ (* (modulo num 10) (expt 10 i))
             (volteando (truncate (/ num 10)) (- i 1))))))

(define (num_equilibrado num)
  (cons 'No
        (cons num 
              (cons '=
                    (cons num
                          (cons '+ 
                                (cons (voltea num) 
                                      (cons '--> 
                                            (cons (+ num (voltea num)) '())))))))))

(define (lista_num_equili limite)
      (lista_num 1000 limite))

(define (lista_num ini limite)
  (cond
    ((> ini limite) '())
    (else (cons (num_equilibrado ini)
                (lista_num (+ ini 1) limite)))))