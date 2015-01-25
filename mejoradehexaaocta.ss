#lang scheme
(define (hexa_a_binario num)
  (cond
    ((equal? num "0") 0)
    ((equal? num "1") 1)
    ((equal? num "2") 10)
    ((equal? num "3") 11)
    ((equal? num "4") 100)
    ((equal? num "5") 101)
    ((equal? num "6") 110)
    ((equal? num "7") 111)
    ((equal? num "8") 1000)
    ((equal? num "9") 1001)
    ((equal? num "A") 1010)
    ((equal? num "B") 1011)
    ((equal? num "C") 1100)
    ((equal? num "D") 1101)
    ((equal? num "E") 1110)
    ((equal? num "F") 1111)))

(define (binario_a_octal num)
  (cond
    ((equal? num 0) 0)
    ((equal? num 1) 1)
    ((equal? num 10) 2)
    ((equal? num 11) 3)
    ((equal? num 100) 4)
    ((equal? num 101) 5)
    ((equal? num 110) 6)
    ((equal? num 111) 7)
  ))

(define (junta_binario num)
  (juntando num (- (string-length num) 1) (string-length num) 0))

(define (juntando num i j k)
  (cond
    ((< i 0) 0)
    (else (+ (* (hexa_a_binario (substring num i j)) (expt 10 k))
             (juntando num (- i 1) (- j 1) (+ k 4))))))

(define (conversion num)
  (convirtiendo (junta_binario num) 0))


(define (convirtiendo num i)
  (cond
    ((= num 0) 0)
    (else (+ (* (binario_a_octal (modulo num 1000)) (expt 10 i))
             (convirtiendo (truncate (/ num 1000)) (+ i 1)))) 
  ))

(define (conversion_hexaoctal num_s)
  (cons num_s (cons 'hexadecimal (cons 'a (cons 'binario (cons '= (cons (junta_binario num_s) (cons 'a (cons 'octal (cons '= (cons (conversion num_s) '()))))))))))
)
