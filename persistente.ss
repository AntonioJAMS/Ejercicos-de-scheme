#lang scheme
(define (cuenta num)
  (cond
    ((= num 0) 0)
    (else (+ 1 (cuenta (truncate (/ num 10)))))))

(define (multiplica num)
  (cond
    ((= num 0) 1)
    (else (* (modulo num 10) (multiplica (truncate (/ num 10)))))))

(define (numpersistente num)
  (persistente 0 num))

(define (persistente i num)
  (cond
    ((= (cuenta num) 1) 
     (cons num (cons 'persistencia (cons '= (cons i '())))))
    ((= (multiplica num) 0) 
     (cons num (cons '- (cons 0 (cons 'persistencia (cons '= (cons (+ i 1) '())))))))
    (else (cons num 
                (cons '-
                      (persistente (+ i 1) (multiplica num)))))))

(define (numerospersistentes limite)
  (numpersistentes 10 limite))

(define (numpersistentes i limite)
  (cond
    ((> i limite) '())
    (else (cons (numpersistente i)
                (numpersistentes (+ i 1) limite)))))