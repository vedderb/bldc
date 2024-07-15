

(define a 1u32)
(define b 2u64)
(define c 1.0f32)
(define d 2.0f64)

(move-to-flash a b c d)

(check (and (= a 1)
            (= a 1)
            (= c 1.0)
            (= d 2.0)))
