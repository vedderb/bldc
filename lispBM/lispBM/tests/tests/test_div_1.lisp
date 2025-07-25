
(define r1 (eq (/ 2b 2b) 1b))
(define r2 (eq (/ 2 2) 1))
(define r3 (eq (/ 2u 2u) 1u))
(define r4 (eq (/ 2i32 2i32) 1i32))
(define r5 (eq (/ 2u32 2u32) 1u32))
(define r6 (eq (/ 2i64 2i64) 1i64))
(define r7 (eq (/ 2u64 2u64) 1u64))
(define r8 (eq (/ 2.0f32 2.0f32) 1.0f32))
(define r9 (eq (/ 2.0f64 2.0f64) 1.0f64))


(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9))
