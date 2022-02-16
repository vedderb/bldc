(define a (cons 1 2))

(setcdr a (cons 7 8))

(= a (cons 1 (cons 7 8)))
