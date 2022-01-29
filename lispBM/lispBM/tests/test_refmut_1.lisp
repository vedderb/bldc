

(define a (cons 1 2))

(set-car a 199)

(and (= (car a) 199) (= (cdr a) 2))

