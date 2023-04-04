
(defun f (x) (+ x 1))

(move-to-flash f)

(check (= (f 1) 2))
