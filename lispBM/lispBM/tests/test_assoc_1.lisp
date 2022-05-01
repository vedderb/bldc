(define alist (list '(p . a) '(q . b) '(r . c) '(s . d)))

(and (eq (assoc alist 'q) 'b)
     (eq (assoc alist 's) 'd))
