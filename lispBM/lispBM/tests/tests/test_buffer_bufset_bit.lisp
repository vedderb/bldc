

(define b (bufcreate 16))

(bufset-bit b 0 1)
(bufset-bit b 8 1)

(define r1 (= 1 (bufget-i8 b 0)))
(define r2 (= 1 (bufget-i8 b 1)))
(define r3 (= 0 (bufget-i8 b 2)))

(define eerr '(exit-error eval_error))
(define r4 (eq eerr (trap (bufset-bit b))))
(define r5 (eq eerr (trap (bufset-bit b 1))))
(define r6 (eq eerr (trap (bufset-bit b 1 2 3))))

(free b)

(define terr '(exit-error type_error))
(define r7 (eq terr (trap (bufset-bit b 0 1))))

(check (and r1 r2 r3 r4 r5 r6 r7))
