
(define b (bufcreate 15))

(bufset-u24 b 0 500000)
(bufset-u24 b 3 400000)
(bufset-u24 b 6 300000)
(bufset-u24 b 9 200000)
(bufset-u24 b 12 100000)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-u24 b 15))))
(define r2 (= 100000 (bufget-u24 b 12)))
(define r3 (= 200000 (bufget-u24 b 9)))
(define r4 (= 300000 (bufget-u24 b 6)))
(define r5 (= 400000 (bufget-u24 b 3)))
(define r6 (= 500000 (bufget-u24 b 0)))

(check (and r1 r2 r3 r4 r5))
