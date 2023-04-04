(define alist (list '(p . a) '(q . b) '(r . c) '(s . d)))

(setvar 'alist (acons 't 'e alist))

(check (and (eq (cossa alist 'b) 'q)
            (eq (cossa alist 'd) 's)
            (eq (cossa alist 'e) 't)))
