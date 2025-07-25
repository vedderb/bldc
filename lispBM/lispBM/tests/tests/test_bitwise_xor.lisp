
(define r1 (eq (bitwise-xor 3 1) 2))
(define r2 (eq (bitwise-xor 3u 1) 2u))
(define r3 (eq (bitwise-xor 3i32 1) 2i32))
(define r4 (eq (bitwise-xor 3u32 1) 2u32))
(define r5 (eq (bitwise-xor 3i64 1) 2i64))
(define r6 (eq (bitwise-xor 3u64 1) 2u64))

(define terr '(exit-error type_error))
(define eerr '(exit-error eval_error))
(define r7 (eq terr (trap (bitwise-xor 1 'apa))))
(define r8 (eq terr (trap (bitwise-xor 'apa 1))))

(define r9 (eq terr (trap (bitwise-xor 1.0f32 1))))
(define r10 (eq terr (trap (bitwise-xor 1.0f64 1))))

(define r11 (eq eerr (trap (bitwise-xor 1))))



(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11))
