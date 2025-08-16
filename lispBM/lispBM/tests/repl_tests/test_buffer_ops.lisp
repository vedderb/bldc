(define buf (bufcreate 10))
(bufset-u8 buf 0 42)
(bufset-u16 buf 2 1000)
(bufset-u32 buf 4 70000)

(define val1 (bufget-u8 buf 0))
(define val2 (bufget-u16 buf 2))
(define val3 (bufget-u32 buf 4))

(if (and (= val1 42)
         (= val2 1000)
         (= val3 70000))
    (print "SUCCESS")
  (print "FAILURE"))
