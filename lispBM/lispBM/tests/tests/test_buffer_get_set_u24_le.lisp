
(define b (bufcreate 15))

(bufset-u24 b 0 500000 'little-endian)
(bufset-u24 b 3 400000 'little-endian)
(bufset-u24 b 6 300000 'little-endian)
(bufset-u24 b 9 200000 'little-endian)
(bufset-u24 b 12 100000 'little-endian)

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-u24 b 15 'little-endian))))
(define r2 (= 100000 (bufget-u24 b 12 'little-endian)))
(define r3 (= 200000 (bufget-u24 b 9 'little-endian)))
(define r4 (= 300000 (bufget-u24 b 6 'little-endian)))
(define r5 (= 400000 (bufget-u24 b 3 'little-endian)))
(define r6 (= 500000 (bufget-u24 b 0 'little-endian)))

(check (and r1 r2 r3 r4 r5))
