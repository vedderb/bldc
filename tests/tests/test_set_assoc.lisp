(define alist nil)

(define l1 (trap (setassoc alist '(1 . apa))))

(check (eq l1 '(exit-error eval_error)))
