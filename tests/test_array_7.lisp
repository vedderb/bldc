(define arr (bufcreate 10))

(bufset-f32 arr 5 3.14)

(check (= (bufget-f32 arr 5) 3.14))
