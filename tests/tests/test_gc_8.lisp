
(define arr1 [1 2 3 4])
(define arr2 [5 6 7 8])
(define tree (cons arr1 arr2))

(gc)

(check (eq tree '([1 2 3 4] . [5 6 7 8])))
