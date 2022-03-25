(define arr (array-create type-double 10))

(array-write arr 5 3.14f64)

(= (array-read arr 5) 3.14f64)
