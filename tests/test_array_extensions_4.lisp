(define arr (array-create type-byte 16))

(bufset-f32 arr 3.14 0)
(bufset-f32 arr 666.666 4)
(bufset-f32 arr 100 8)
(bufset-f32 arr 42 12)

(and (num-eq (bufget-f32 arr 0) 3.14)
     (num-eq (bufget-f32 arr 4) 666.666)
     (num-eq (bufget-f32 arr 8) 100)
     (num-eq (bufget-f32 arr 12) 42))
