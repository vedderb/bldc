
(define r1 (and (eq (to-byte 1u) 1b)
                (eq (to-byte 1)  1b)
                (eq (to-byte 1i32) 1b)
                (eq (to-byte 1u32) 1b)
                (eq (to-byte 1i64) 1b)
                (eq (to-byte 1u64) 1b)
                (eq (to-byte 1.0f32) 1b)
                (eq (to-byte 1.0f64) 1b)))

(define r2 (eq '(exit-error eval_error) (trap (to-byte))))

(check (and r1 r2))
