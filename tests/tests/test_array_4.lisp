(define arr (bufcreate 10))

(bufset-i8 arr 5 77)

(check (= (bufget-i8 arr 5) 77))
