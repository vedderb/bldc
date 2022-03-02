(define arr (array-create type-byte 16))

(bufset-f32 arr 0 3.14)
(bufset-f32 arr 4 666.666)
(bufset-f32 arr 8 100)
(bufset-f32 arr 12 42)

(and (num-eq (bufget-f32 arr 0) 3.14)
     (num-eq (bufget-f32 arr 4) 666.666)
     (num-eq (bufget-f32 arr 8) 100)
     (num-eq (bufget-f32 arr 12) 42))
