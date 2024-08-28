
(DEFINE R1 (+ 1 2 3))
(define r2 (+ 1 2 3))

(check (and (= r1 r2) (= r1 R1) (= R1 R2) (= r2 R2))) 
