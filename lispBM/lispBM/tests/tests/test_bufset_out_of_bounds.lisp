
(define b (bufcreate 10))

(define eerr '(exit-error eval_error))

(define is-err (lambda (x) (eq eerr x)))

(define r1 (is-err (trap (bufset-i8 b 10 1))))
(define r2 (is-err (trap (bufset-i16 b 10 1))))
(define r3 (is-err (trap (bufset-i32 b 10 1))))

(define r4 (is-err (trap (bufset-u8 b 10 1))))
(define r5 (is-err (trap (bufset-u16 b 10 1))))
(define r6 (is-err (trap (bufset-u24 b 10 1))))
(define r7 (is-err (trap (bufset-u32 b 10 1))))

(define r8 (is-err  (trap (bufset-f32 b 10 1.0))))


(check (and r1 r2 r3 r4 r5 r6 r7 r8))

