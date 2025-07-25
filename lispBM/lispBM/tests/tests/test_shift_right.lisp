(define r1 (eq (shr 2 1) 1))
(define r2 (eq (shr 2u 1) 1u))
(define r3 (eq (shr 2i32 1) 1i32))
(define r4 (eq (shr 2u32 1) 1u32))
(define r5 (eq (shr 2i64 1) 1i64))
(define r6 (eq (shr 2u64 1) 1u64))

(define terr '(exit-error type_error))
(define eerr '(exit-error eval_error))
(define r7 (eq terr (trap (shr 1 'apa))))
(define r8 (eq terr (trap (shr 'apa 1))))

(define r9 (eq terr (trap (shr 1.0f32 1))))
(define r10 (eq terr (trap (shr 1.0f64 1))))

(define r11 (eq eerr (trap (shr 1))))



(check (and  r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11))
