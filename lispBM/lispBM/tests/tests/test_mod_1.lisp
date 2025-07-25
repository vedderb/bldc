
(define r1 (eq (mod 5b 4b) 1b))
(define r2 (eq (mod 5 4) 1))
(define r3 (eq (mod 5u 4u) 1u))
(define r4 (eq (mod 5i32 4i32) 1i32))
(define r5 (eq (mod 5u32 4u32) 1u32))
(define r6 (eq (mod 5i64 4i64) 1i64))
(define r7 (eq (mod 5u64 4u64) 1u64))
(define r8 (eq (mod 5.0f32 4.0f32) 1.0f32))
(define r9 (eq (mod 5.0f64 4.0f64) 1.0f64))


(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9))
