(define arr (array-create type-float 10))

(array-write arr 5 3.14)

(= (array-read arr 5) 3.14)
