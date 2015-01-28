#lang scheme
(define (menor num1 num2)
  (cond
    ((< num1 num2) num1)
    (else num2)))

(define (fraccion num1 num2)
  (reducida 2 num1 num2))

(define (reducida i num1 num2)
  (cond
    ((> i (menor num1 num2)) (cons num1 (cons num2 '())))
    ((and (= (modulo num1 i) 0) (= (modulo num2 i) 0))
     (cons (/ num1 i) (cons (/ num2 i) '())))
    (else (reducida (+ i 1) num1 num2))))
