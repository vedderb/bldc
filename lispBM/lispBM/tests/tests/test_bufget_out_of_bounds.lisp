
(define b (bufcreate 10))

(define eerr '(exit-error eval_error))

(define is-err (lambda (x) (eq eerr x)))

(define r1 (is-err (trap (bufget-i8 b 10))))
(define r2 (is-err (trap (bufget-i16 b 10))))
(define r3 (is-err (trap (bufget-i32 b 10))))

(define r4 (is-err (trap (bufget-u8 b 10))))
(define r5 (is-err (trap (bufget-u16 b 10))))
(define r6 (is-err (trap (bufget-u24 b 10))))
(define r7 (is-err (trap (bufget-u32 b 10))))

(define r8 (is-err  (trap (bufget-f32 b 10))))


(check (and r1 r2 r3 r4 r5 r6 r7 r8))

