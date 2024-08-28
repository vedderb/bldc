
(define r1 (eq (- 0b 1b) 255b))
(define r2 (eq (- 0 1) -1))
(define r3 (eq (- 0u 1u) -1u))
(define r4 (eq (- 0i32 1i32) -1i32))
(define r5 (eq (- 0u32 1u32) -1u32))
(define r6 (eq (- 0i64 1i64) -1i64))
(define r7 (eq (- 0u64 1u64) -1u64))
(define r8 (eq (- 0.0f32 1.0f32) -1.0f32))
(define r9 (eq (- 0.0f64 1.0f64) -1.0f64))


(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9))
