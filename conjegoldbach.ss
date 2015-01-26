#lang scheme
(define (primo num)
  (if (= (veriprimo 1 num) 2)
      1
      0))

(define (veriprimo i num)
  (cond
    ((> i num) 0)
    ((= (modulo num i) 0) (+ 1 (veriprimo (+ i 1) num)))
    (else (veriprimo (+ i 1) num))))

(define (siguiente_primo num)
  (siguiente 0 num))

(define (siguiente i num)
  (cond
    ((= i 2) (- num 1))
    ((= (primo num) 1) (siguiente (+ i 1) (+ num 1)))
    (else (siguiente i (+ num i)))))

(define (conjetura limite)
  (conje 4 limite))

(define (conje par limite)
  (cond
    ((> par limite) '())
    (else (cons (numpar par)
                (conje (+ par 2) limite)))))

(define (numpar par)
  (numnumpar par 2))

(define (numnumpar par ini)
  (cond
    ((> ini par) '())
    ((equal? (buscando par ini 2) '()) (numnumpar par (+ ini 1)))
    ((not (equal? (buscando par ini 2) '())) (buscando par ini 2))
    (else (cons (buscando par ini 2)
                (conje par (+ ini 1))))))
    
(define (buscando par ini i)
  (cond
    ((> (+ ini i) par) '())
    ((and (= (primo ini) 1) (= (primo i) 1) (= par (+ ini i))) 
     (cons par (cons '= (cons (cons ini (cons '+ (cons i '()))) '()))))
    (else (buscando par ini (+ i 1)))))