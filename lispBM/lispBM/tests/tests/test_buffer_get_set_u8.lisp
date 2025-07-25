
(define b (bufcreate 5))

(bufset-u8 b 0 5)
(bufset-u8 b 1 4)
(bufset-u8 b 2 3)
(bufset-u8 b 3 2)
(bufset-u8 b 4 1)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-u8 b 5))))
(define r2 (= 1 (bufget-u8 b 4)))
(define r3 (= 2 (bufget-u8 b 3)))
(define r4 (= 3 (bufget-u8 b 2)))
(define r5 (= 4 (bufget-u8 b 1)))
(define r6 (= 5 (bufget-u8 b 0)))

(check (and r1 r2 r3 r4 r5))
