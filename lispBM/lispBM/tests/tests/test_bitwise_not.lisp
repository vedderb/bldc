
(define r1 (eq (bitwise-not (to-i 0xFFFFFFFFFFFFFFFFu64)) 0))
(define r2 (eq (bitwise-not (to-u 0xFFFFFFFFFFFFFFFFu64)) 0u))
(define r3 (eq (bitwise-not (to-i32 0xFFFFFFFFFFFFFFFFu64)) 0i32))
(define r4 (eq (bitwise-not (to-u32 0xFFFFFFFFFFFFFFFFu64)) 0u32))
(define r5 (eq (bitwise-not (to-i64 0xFFFFFFFFFFFFFFFFu64)) 0i64))
(define r6 (eq (bitwise-not (to-u64 0xFFFFFFFFFFFFFFFFu64)) 0u64))

(define terr '(exit-error type_error))
(define eerr '(exit-error eval_error))
(define r7 (eq terr (trap (bitwise-not 'apa))))

(define r8 (eq terr (trap (bitwise-not 1.0f32))))
(define r9 (eq terr (trap (bitwise-not 1.0f64))))

(define r10 (eq eerr (trap (bitwise-not))))



(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r1))
