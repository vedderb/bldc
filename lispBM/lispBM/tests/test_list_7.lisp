
(define f (lambda () (list '(a . 1) '(b . 2) '(c . 3) '(d . 4))))

(setassoc (f) 'b 77)

(check (eq  (f) (list '(a . 1) '(b . 2) '(c . 3) '(d . 4))))

