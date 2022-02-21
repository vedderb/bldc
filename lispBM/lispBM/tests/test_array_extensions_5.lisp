
(define arr (array-create type-byte 16))

(free arr)

(and (= (car arr) nil)
     (= (cdr arr) nil))
