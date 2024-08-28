
(define r1 (eq (bitwise-and 3 3) 3))
(define r2 (eq (bitwise-and 3u 3) 3u))
(define r3 (eq (bitwise-and 3i32 3) 3i32))
(define r4 (eq (bitwise-and 3u32 3) 3u32))
(define r5 (eq (bitwise-and 3i64 3) 3i64))
(define r6 (eq (bitwise-and 3u64 3) 3u64))

(define terr '(exit-error type_error))
(define eerr '(exit-error eval_error))
(define r7 (eq terr (trap (bitwise-and 1 'apa))))
(define r8 (eq terr (trap (bitwise-and 'apa 1))))

(define r9 (eq terr (trap (bitwise-and 1.0f32 1))))
(define r10 (eq terr (trap (bitwise-and 1.0f64 1))))

(define r11 (eq eerr (trap (bitwise-and 1))))



(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11))
