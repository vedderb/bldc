(define arr (bufcreate 10))

(bufset-u16 arr 0 65535)
(bufset-u16 arr 2 10)
(bufset-u16 arr 4 11700)

(check (and (= (bufget-u16 arr 0) 65535)
            (= (bufget-u16 arr 2) 10) 
            (= (bufget-u16 arr 4) (mod 11700 65536))))
