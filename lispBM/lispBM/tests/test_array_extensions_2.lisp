
(define arr (array-create type-byte 16))

(bufset-u32 arr 16777215 0)
(bufset-u32 arr 0xFFFFFFFF 4)
(bufset-u32 arr 10 8)
(bufset-u32 arr 0xDEADBEEF 12)

(and (num-eq (bufget-u32 arr 0) 16777215)
     (num-eq (bufget-u32 arr 4) 0xFFFFFFFF)
     (num-eq (bufget-u32 arr 8) 10)
     (num-eq (bufget-u32 arr 12) 0xDEADBEEF))
     
     
