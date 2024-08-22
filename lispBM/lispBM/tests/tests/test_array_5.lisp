(define arr (bufcreate 10))

(bufset-u32 arr 5 1234 )

(check (= (bufget-u32 arr 5) 1234))
