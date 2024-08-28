
(define a (bufcreate 16))
(define b [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])

(move-to-flash b)

(define r1 (bufcpy a 0 b 0 16))

(define terr '(exit-error type_error))

(define r2 (eq terr (trap (bufcpy b 0 a 0 16))))

(define r3 (eq terr (trap (bufset-i8 b 1 77))))
(define r4 (= (bufget-i8 b 1) 1))


(check (and r1 r2 r3 r4))
