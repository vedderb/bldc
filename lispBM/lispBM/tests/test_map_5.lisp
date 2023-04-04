(define ls (list (list 1 2 3) (list 2 3 4) (list 3 4 5)))
(define rs (list (list 3 2 1) (list 4 3 2) (list 5 4 3)))

(defun f (x) (+ x 1))

(check (eq (map reverse ls) rs))
