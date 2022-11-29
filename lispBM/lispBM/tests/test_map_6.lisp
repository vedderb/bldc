
(defun f (x) (+ x 1))

(define myfun (map f))

(define ls (list (list 1 2 3) (range 2 5)))
(define rs '((2 3 4) (3 4 5)))

(eq (map myfun ls) rs)
