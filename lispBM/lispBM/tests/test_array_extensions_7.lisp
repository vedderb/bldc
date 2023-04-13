
(define arr (bufcreate 19))

(bufset-u8 arr 0 255)
(bufset-u8 arr 1 127)
(bufset-u8 arr 18 1100)

(check (and (= (bufget-u8 arr 0) 255)
            (= (bufget-u8 arr 1) 127) 
            (= (bufget-u8 arr 18) (mod 1100 256))))
