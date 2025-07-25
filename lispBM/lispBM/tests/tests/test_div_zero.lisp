
(define r1 (trap (/ 1b 0b)))
(define r2 (trap (/ 1 0)))
(define r3 (trap (/ 1u 0)))
(define r4 (trap (/ 1i32 0)))
(define r5 (trap (/ 1u32 0)))
(define r6 (trap (/ 1i64 0)))
(define r7 (trap (/ 1u64 0)))
(define r8 (trap (/ 1.0f32 0)))
(define r9 (trap (/ 1.0f64 0)))



                 
(check (and (eq '(exit-error division_by_zero) r1)
            (eq '(exit-error division_by_zero) r2)
            (eq '(exit-error division_by_zero) r3)
            (eq '(exit-error division_by_zero) r4)
            (eq '(exit-error division_by_zero) r5)
            (eq '(exit-error division_by_zero) r6)
            (eq '(exit-error division_by_zero) r7)
            (eq '(exit-error division_by_zero) r8)
            (eq '(exit-error division_by_zero) r9)))
