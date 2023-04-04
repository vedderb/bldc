
(define ls (list (list 1 2 3) (list 2 3 4) (list 3 4 5)))
(define rs (list (list 2 3 4) (list 3 4 5) (list 4 5 6)))

(defun f (x) (+ x 1))

(check (eq (map (lambda (x) (map f x)) ls) rs))
