
(define m (mutex-create))

(mutex-lock m)

(define l (length (car m)))

(mutex-unlock m)

(define i (length (car m)))

(check (and (= l 1)
            (= i 0)))
