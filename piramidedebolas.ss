#lang scheme

(define (capas)
  (total-capas 1 10000 1)
)
  
(define (total-capas inicio final bolas)
  (cond
    ((> bolas final) (- inicio 1))
    (else (total-capas (+ inicio 1) final (+ bolas (expt (+ inicio 1) 2))))
  )
)

(define (bolas)
  (total-bolas 1 10000 1)
)

(define (total-bolas inicio final bolas)
  (cond
    ((> bolas final) (- 10000 (- bolas (expt inicio 2))))
    (else (total-bolas (+ inicio 1) final (+ bolas (expt (+ inicio 1) 2))))
  )
)

(define (piramide-bolas)
  (cons 'El (cons 'numero (cons 'de (cons 'capas (cons 'es (cons 'de (cons (capas)
  (cons 'y (cons 'sobrarian (cons 'un (cons 'total (cons 'de (cons (bolas) '())))))))))))))
)





;(- 10000 (+ 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 729 784 841 900))
