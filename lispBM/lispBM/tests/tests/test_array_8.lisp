(define arr (bufcreate 10))

(bufset-f32 arr 4 3.14f32)

(check (= (bufget-f32 arr 4) 3.14f32))
