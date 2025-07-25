
(define err '(exit-error eval_error))

(define is-error (lambda (x) (eq err x)))

(define r1 (is-error (trap (to-i 1 2))))
(define r2 (is-error (trap (to-u 1 2))))
(define r3 (is-error (trap (to-i32 1 2))))
(define r4 (is-error (trap (to-u32 1 2))))
(define r5 (is-error (trap (to-i64 1 2))))
(define r6 (is-error (trap (to-u64 1 2))))
(define r7 (is-error (trap (to-float 1 2))))
(define r8 (is-error (trap (to-double 1 2))))


(check (and r1 r2 r3 r4 r5 r6 r7 r8))
