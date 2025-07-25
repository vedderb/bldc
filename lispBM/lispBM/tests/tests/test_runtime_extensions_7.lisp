
(define type-ok (lambda (x)
                  (eq (type-of x) type-u)))

(define r1 (type-ok (symtab-size)))
(define r2 (type-ok (symtab-size-flash)))
(define r3 (type-ok (symtab-size-names)))
(define r4 (type-ok (symtab-size-names-flash)))


(check (and r1 r2 r3 r4))
