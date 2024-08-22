(define alist (list '(1 . a) '(2 . b) '(3 . c) '(4 . d)))

(check (and (eq (cossa alist 'c) 3)
            (eq (cossa alist 'a) 1)))
