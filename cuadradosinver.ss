#lang scheme
(define (cuenta num)
	(cond
		((= num 0) 0)
		(else (+ 1 (cuenta (truncate (/ num 10)))))))

(define (tomanum num)
	(tomandonum 1 (+ (/ (cuenta (expt num 2)) 2) 1) 3 (expt num 2)))

(define (tomandonum i c p num)
	(cond
		((> i 4) 0)
		(else (+ (* (modulo (truncate (/ num (expt 10 c))) 10) 
					(expt 10 p))
			     (tomandonum (+ i 1) (- c 1) (- p 1) num)))))

(define (resultado i num)
  (cons 'No
        (cons i
              (cons '--
	(cons 'X=
		(cons num
                      (cons '--
			(cons 'X2=
				(cons (expt num 2)
                                      (cons '--
					(cons 'No.aleatorio 
                                                 (cons (/ (* 1.0 (tomanum num)) 10000) '())))))))))))) 

(define (cuadradosConLimite limite num)
	(cuadradosCon 1 limite num))

(define (cuadradosCon c limite num)
	(cond 
		((> c limite) '())
		(else (cons (resultado c num)
					(cuadradosCon (+ c 1) limite (tomanum num))))))
(define (cuadradosSinLimite num)
  (cuadradosSin 1 num))

(define (cuadradosSin c num)
	(cond 
		((< (cuenta (expt num 2)) 4) '())
		(else (cons (resultado c num)
					(cuadradosSin (+ c 1) (tomanum num))))))


