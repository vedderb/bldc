(define arr (array-create type-byte 16))

(bufset-i32 arr 0 16777215)
(bufset-i32 arr 4 0xFFFFFFFF)
(bufset-i32 arr 8 10)
(bufset-i32 arr 12 0xDEADBEEF)

(and (num-eq (bufget-i32 arr 0) 16777215)
     (num-eq (bufget-i32 arr 4) 0xFFFFFFFF)
     (num-eq (bufget-i32 arr 8) 10)
     (num-eq (bufget-i32 arr 12) 0xDEADBEEF))
