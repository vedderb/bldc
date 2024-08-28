

(define b (bufcreate 10))

(bufset-i16 b 0 500)
(bufset-i16 b 2 400)
(bufset-i16 b 4 300)
(bufset-i16 b 6 -400)
(bufset-i16 b 8 -500)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-i16 b 10))))
(define r2 (= -500 (bufget-i16 b 8)))
(define r3 (= -400 (bufget-i16 b 6)))
(define r4 (= 300 (bufget-i16 b 4)))
(define r5 (= 400 (bufget-i16 b 2)))
(define r6 (= 500 (bufget-i16 b 0)))

(check (and r1 r2 r3 r4 r5))
