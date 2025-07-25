(define dm (dm-create 128))

(define a (bufcreate dm 40))

(loopfor i 0 (< i 1000) (+ i 1)
         {
         (setq a (bufcreate dm 40))
         (bufset-i8 a 0 10)
         (bufset-i8 a 1 2)
         }
         )

(check (and (= (bufget-i8 a 0) 10)
            (= (bufget-i8 a 1) 2)))
