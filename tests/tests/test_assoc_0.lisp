(define alist (list '(1 . a) '(2 . b) '(3 . c) '(4 . d)))

(check (and (eq (assoc alist 3) 'c)
            (eq (assoc alist 1) 'a)))
