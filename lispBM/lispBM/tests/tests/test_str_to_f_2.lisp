(define r1 (eq (str-to-f "2.5") 2.500000f32))

(define r2 (eq (str-to-f "0.0025e3") 2.500000f32))

(check (and r1 r2))



