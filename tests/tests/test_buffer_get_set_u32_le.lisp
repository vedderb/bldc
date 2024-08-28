

(define b (bufcreate 20))

(bufset-u32 b 0 500000 'little-endian)
(bufset-u32 b 4 400000 'little-endian)
(bufset-u32 b 8 300000 'little-endian)
(bufset-u32 b 12 200000 'little-endian)
(bufset-u32 b 16 100000 'little-endian) 

(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (bufget-u32 b 20 'little-endian))))
(define r2 (= 100000 (bufget-u32 b 16 'little-endian)))
(define r3 (= 200000 (bufget-u32 b 12 'little-endian)))
(define r4 (= 300000 (bufget-u32 b 8 'little-endian)))
(define r5 (= 400000 (bufget-u32 b 4 'little-endian)))
(define r6 (= 500000 (bufget-u32 b 0 'little-endian)))

(check (and r1 r2 r3 r4 r5))
