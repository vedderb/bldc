(define r1  (eq (str-to-i "123") 123i32))

(define r2 (eq (str-to-i "a" 16) 10i32))

(define r3 (eq (str-to-i "0xa") 10i32))

(check (and r1 r2 r3))
