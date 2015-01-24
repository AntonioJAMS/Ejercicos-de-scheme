#lang scheme
(define (junta num num2)
  (+ (* num (expt 10 2)) num2))

(define (jugadores)
  (jugador 1 99))

(define (jugador ini fin)
  (cond
    ((> ini fin ) '())
    ((equal? (buscar ini fin 1) '()) (jugador (+ ini 1) fin))
    (else (cons (buscar ini fin 1)
                (jugador (+ ini 1) fin)))))

(define (buscar ini fin i)
  (cond
    ((> i fin) '())
    ((= (junta ini i) (expt (+ ini i) 2)) 
     (cons (cons ini 
            (cons 'y 
             (cons i 
              (cons 'juntados 
               (cons (junta ini i)
                (cons 'la
                 (cons 'suma
                  (cons 'al 
                   (cons 'cuadrado
                    (cons (expt (+ ini i) 2) '()))))))))))
           (buscar ini fin (+ i 1))))
    (else (buscar ini fin (+ i 1)))))