
(define b (bufcreate 5))

(bufset-i8 b 0 5 'little-endian)
(bufset-i8 b 1 4 'little-endian)
(bufset-i8 b 2 3 'little-endian)
(bufset-i8 b 3 -2 'little-endian)
(bufset-i8 b 4 -1 'little-endian)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-i8 b 5 'little-endian))))
(define r2 (= -1 (bufget-i8 b 4 'little-endian)))
(define r3 (= -2 (bufget-i8 b 3 'little-endian)))
(define r4 (= 3 (bufget-i8 b 2 'little-endian)))
(define r5 (= 4 (bufget-i8 b 1 'little-endian)))
(define r6 (= 5 (bufget-i8 b 0 'little-endian)))

(check (and r1 r2 r3 r4 r5))
