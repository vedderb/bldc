(define v1 (list 1 2 3 4))
(define v2 (list 5 6 7 8))

(define fv1 (flatten v1))
(define fv2 (flatten v2))

(define tree (cons fv1 fv2))

(gc)

(check (and
        (eq (unflatten (car tree)) (list 1 2 3 4))
        (eq (unflatten (cdr tree)) (list 5 6 7 8))))
