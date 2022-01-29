

(define a (cons 1 2))

(set-cdr a 199)

(and (= (car a) 1) (= (cdr a) 199))

