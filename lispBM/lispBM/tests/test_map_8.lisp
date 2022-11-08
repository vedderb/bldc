(defun f (x) (+ x 1))


(define ls (list (list (list 1 2 3) (range 2 5))))
(define rs '(((2 3 4) (3 4 5))))

(eq (map (map (map f)) ls) rs)
