
(define arr (array-create type-byte 16))

(free arr)

(and (eq (car arr) nil)
     (eq (cdr arr) nil))
