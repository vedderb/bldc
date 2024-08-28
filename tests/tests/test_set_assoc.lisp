(define alist nil)

(define l1 (setassoc alist '(1 . apa)))

(check (eq (assoc l1 1) 'apa))
