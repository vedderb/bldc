

(define b (bufcreate 10))

(bufset-i16 b 0 500 'little-endian)
(bufset-i16 b 2 400 'little-endian)
(bufset-i16 b 4 300 'little-endian)
(bufset-i16 b 6 -400 'little-endian)
(bufset-i16 b 8 -500 'little-endian)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-i16 b 10 'little-endian))))
(define r2 (= -500 (bufget-i16 b 8 'little-endian)))
(define r3 (= -400 (bufget-i16 b 6 'little-endian)))
(define r4 (= 300 (bufget-i16 b 4 'little-endian)))
(define r5 (= 400 (bufget-i16 b 2 'little-endian)))
(define r6 (= 500 (bufget-i16 b 0 'little-endian)))

(check (and r1 r2 r3 r4 r5))
