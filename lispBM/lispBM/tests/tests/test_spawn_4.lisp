(defun f (a b c) (list a b c))

(defun g (a b c) (check (eq (f a b c) (list 1 2 3))))

(spawn g 1 2 3)
