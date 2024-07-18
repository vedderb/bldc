
(define a 1u32)
(define b 1.0f32)
(define c 1u64)
(define d 1.0f64)



(move-to-flash a)
(move-to-flash b)

(check (and (= 1u32 a)
	    (= 1.0f32 b)
	    (= 1u64 c)
	    (= 1.0f64 d)))
