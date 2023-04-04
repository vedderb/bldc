
(defun f (x) (+ x 1))

(def a '(1 2 3 4 5 6))

(move-to-flash f a)

(check (eq (map f a) '(2 3 4 5 6 7)))
