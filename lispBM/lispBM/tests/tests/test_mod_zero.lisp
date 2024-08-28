
(define r1 (trap (mod 1b 0b)))
(define r2 (trap (mod 1 0)))
(define r3 (trap (mod 1u 0)))
(define r4 (trap (mod 1i32 0)))
(define r5 (trap (mod 1u32 0)))
(define r6 (trap (mod 1i64 0)))
(define r7 (trap (mod 1u64 0)))
(define r8 (trap (mod 1.0f32 0)))
(define r9 (trap (mod 1.0f64 0)))



                 
(check (and (eq '(exit-error division_by_zero) r1)
            (eq '(exit-error division_by_zero) r2)
            (eq '(exit-error division_by_zero) r3)
            (eq '(exit-error division_by_zero) r4)
            (eq '(exit-error division_by_zero) r5)
            (eq '(exit-error division_by_zero) r6)
            (eq '(exit-error division_by_zero) r7)
            (eq '(exit-error division_by_zero) r8)
            (eq '(exit-error division_by_zero) r9)))
