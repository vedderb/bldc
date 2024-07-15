(define alist (list '(p . a) '(q . b) '(r . c) '(s . d)))

(check (and (eq (cossa alist 'b) 'q)
            (eq (cossa alist 'd) 's)))
