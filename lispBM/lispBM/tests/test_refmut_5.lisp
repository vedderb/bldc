(define a (cons 1 2))

(set-car a (cons 7 8))

(= a (cons (cons 7 8) 2))
