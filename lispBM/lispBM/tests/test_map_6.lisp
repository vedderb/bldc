
(defun f (x) (+ x 1))

(define myfun (lambda (ls) (map f ls)))

(define ls (list (list 1 2 3) (range 2 5)))
(define rs '((2 3 4) (3 4 5)))

(check (eq (map myfun ls) rs))
