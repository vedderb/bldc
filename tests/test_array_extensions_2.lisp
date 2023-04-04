
(define arr (bufcreate 16))

(bufset-u32 arr 0 16777215)
(bufset-u32 arr 4 0xFFFFFFFF)
(bufset-u32 arr 8 10)
(bufset-u32 arr 12 0xDEADBEEF)

(check (and (= (bufget-u32 arr 0) 16777215)
            (= (bufget-u32 arr 4) 0xFFFFFFFF)
            (= (bufget-u32 arr 8) 10)
            (= (bufget-u32 arr 12) 0xDEADBEEF)))
     
     
