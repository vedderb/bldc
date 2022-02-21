(define arr (array-create type-byte 16))

(buffer-append-f32 arr 3.14 0)
(buffer-append-f32 arr 666.666 4)
(buffer-append-f32 arr 100 8)
(buffer-append-f32 arr 42 12)

(and (num-eq (buffer-get-f32 arr 0) 3.14)
     (num-eq (buffer-get-f32 arr 4) 666.666)
     (num-eq (buffer-get-f32 arr 8) 100)
     (num-eq (buffer-get-f32 arr 12) 42))
