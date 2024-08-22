(define arr (bufcreate 10))

(bufset-i32 arr 5 77)

(check (= (bufget-i32 arr 5) 77))
