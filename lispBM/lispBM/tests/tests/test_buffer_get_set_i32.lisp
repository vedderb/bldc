

(define b (bufcreate 20))

(bufset-i32 b 0 500000)
(bufset-i32 b 4 400000)
(bufset-i32 b 8 300000)
(bufset-i32 b 12 -400000)
(bufset-i32 b 16 -500000)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-i32 b 20))))
(define r2 (= -500000 (bufget-i32 b 16)))
(define r3 (= -400000 (bufget-i32 b 12)))
(define r4 (= 300000 (bufget-i32 b 8)))
(define r5 (= 400000 (bufget-i32 b 4)))
(define r6 (= 500000 (bufget-i32 b 0)))

(check (and r1 r2 r3 r4 r5))
