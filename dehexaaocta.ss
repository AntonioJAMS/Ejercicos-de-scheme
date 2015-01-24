#lang scheme
(define (binario numhexa)
  (cond
    ((equal? numhexa "0") 0)
    ((equal? numhexa "1") 1)
    ((equal? numhexa "2") 10)
    ((equal? numhexa "3") 11)
    ((equal? numhexa "4") 100)
    ((equal? numhexa "5") 101)
    ((equal? numhexa "6") 110)
    ((equal? numhexa "7") 111)
    ((equal? numhexa "8") 1000)
    ((equal? numhexa "9") 1001)
    ((equal? numhexa "A") 1010)
    ((equal? numhexa "B") 1011)
    ((equal? numhexa "C") 1100)
    ((equal? numhexa "D") 1101)
    ((equal? numhexa "E") 1110)
    ((equal? numhexa "F") 1111)))

(define (octa numbina)
  (cond
    ((equal? numbina 0) 0)
    ((equal? numbina 1) 1)
    ((equal? numbina 10) 2)
    ((equal? numbina 11) 3)
    ((equal? numbina 100) 4)
    ((equal? numbina 101) 5)
    ((equal? numbina 110) 6)
    ((equal? numbina 111) 7)))

(define (medir numbina)
  (cond
    ((= numbina 0) 0)
    (else (+ 1 (medir (truncate (/ numbina 10)))))))

(define (detres numero)
  (+ (modulo numero 10) 
     (* (modulo (truncate (/ numero 10)) 10) (expt 10 1)) 
     (* (modulo (truncate (/ numero 100)) 10) (expt 10 2))))

(define (numero_a_octal num)
  (a_octal 0 num))

(define (a_octal i num)
  (cond
    ((= num 0) 0)
    (else (+ (* (octa(detres num)) (expt 10 i))
             (a_octal (+ i 1) (truncate (/ num 1000)))))))

(define (a_binario numhexa)
  (binabinario (- (string-length numhexa) 1) (string-length numhexa) 0 numhexa))

(define (binabinario i j k numhexa)
  (cond
    ((< i 0) 0)
    (else (+ (* (binario (substring numhexa i j)) (expt 10 k))
             (binabinario (- i 1) (- j 1) (+ k 4) numhexa)))
  )
)

(define (de_hexa_a_octal num)
  (cons num 
        (cons 'hexadecimal 
              (cons 'a
                    (cons 'binario
                          (cons '=
                                (cons (a_binario num)
                                      (cons 'a
                                            (cons 'octal 
                                                  (cons '= 
                                                        (cons (numero_a_octal (a_binario num)) '())))))))))))
