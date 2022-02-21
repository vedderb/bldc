(define arr (array-create type-byte 16))

(buffer-append-i32 arr 16777215 0)
(buffer-append-i32 arr 0xFFFFFFFF 4)
(buffer-append-i32 arr 10 8)
(buffer-append-i32 arr 0xDEADBEEF 12)

(and (num-eq (buffer-get-i32 arr 0) 16777215)
     (num-eq (buffer-get-i32 arr 4) 0xFFFFFFFF)
     (num-eq (buffer-get-i32 arr 8) 10)
     (num-eq (buffer-get-i32 arr 12) 0xDEADBEEF))
