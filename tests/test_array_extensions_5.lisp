
(define arr (array-create type-byte 16))

(unsafe-free arr)

(and (= (car arr) nil)
     (= (cdr arr) nil))
