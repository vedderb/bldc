

(define a (cons 1 2))

(setcar a 199)

(and (= (car a) 199) (= (cdr a) 2))

