(define arr (bufcreate 16))

(bufset-i32 arr 0 16777215)
(bufset-i32 arr 4 0xFFFFFFFF)
(bufset-i32 arr 8 10)
(bufset-i32 arr 12 0xDEADBEEF)

(if (= (word-size) 8)
    (check (and (= (bufget-i32 arr 0) (to-i32 16777215))
                (= (bufget-i32 arr 4) (to-i32 0xFFFFFFFF))
                (= (bufget-i32 arr 8) (to-i32 10))
                (= (bufget-i32 arr 12) (to-i32 0xDEADBEEF))))
  (check (and (= (bufget-i32 arr 0) 16777215)
                (= (bufget-i32 arr 4) 0xFFFFFFFF)
                (= (bufget-i32 arr 8) 10)
                (= (bufget-i32 arr 12) 0xDEADBEEF)))
  )
    
  
    
