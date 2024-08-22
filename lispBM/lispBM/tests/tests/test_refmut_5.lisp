(define a (cons 1 2))

(setcar a (cons 7 8))

(check (eq a (cons (cons 7 8) 2)))
