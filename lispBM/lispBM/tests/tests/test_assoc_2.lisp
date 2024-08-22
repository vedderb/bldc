(define alist (list '(p . a) '(q . b) '(r . c) '(s . d)))

(setvar 'alist (acons 't 'e alist))

(check (and (eq (assoc alist 'q) 'b)
            (eq (assoc alist 's) 'd)
            (eq (assoc alist 't) 'e)))
