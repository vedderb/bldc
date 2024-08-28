
(define a [1 2 3 4 5 6])
(define b (bufcreate 6))

(bufcpy b 3 a 0 3)

(define r1 (= 0 (bufget-i8 b 0)))
(define r2 (= 0 (bufget-i8 b 1)))
(define r3 (= 0 (bufget-i8 b 2)))
(define r4 (= 1 (bufget-i8 b 3)))
(define r5 (= 2 (bufget-i8 b 4)))
(define r6 (= 3 (bufget-i8 b 5)))

(define eerr '(exit-error eval_error))
(define r7 (eq eerr (trap (bufcpy a b))))

(free b)

(define terr '(exit-error type_error))
(define r8 (eq terr (trap (bufcpy b 3 a 0 3))))


(check (and r1 r2 r3 r4 r5 r6 r7 r8))

