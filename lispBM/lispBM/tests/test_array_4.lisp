(define arr (array-create type-i32 10))

(array-write arr 5 77)

(= (array-read arr 5) 77)
