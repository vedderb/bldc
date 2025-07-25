

(define arr (list-to-array (list 1 2 3 )))

(move-to-flash arr)

(check (eq arr (list-to-array (list 1 2 3))))
