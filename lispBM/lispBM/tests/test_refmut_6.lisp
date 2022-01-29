(define a (cons 1 2))

(set-cdr a (cons 7 8))

(= a (cons 1 (cons 7 8)))
