
(define b (bufcreate 16))

(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

(define r1 (eq eerr (trap (bufclear))))
(define r2 (eq eerr (trap (bufclear b 1 2 3 4))))

(bufclear b)

(define r3 (= (bufget-i8 b 0) 0)) 

(bufclear b 0xF)

(define r4 (= (bufget-i8 b 1) 15))

(bufclear b)
(bufclear b 1 5)
(define r5 (= (bufget-i8 b 1) 0))
(define r6 (= (bufget-i8 b 5) 1))

(bufclear b)
(bufclear b 1 5 5)
(define r7 (= (bufget-i8 b 1) 0))
(define r8 (= (bufget-i8 b 5) 1))
(define r9 (= (bufget-i8 b 11) 0))


(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9))

