(define alist (list '(1 . a) '(2 . b) '(3 . c) '(4 . d)))

(setassoc 'alist 3 'e)

(check (and (eq (cossa alist 'c) 3)
            (eq (cossa alist 'a) 1)))
