
(defun f (x)
  (let ( (g (lambda (y) (rest-args))) )
    (g 1 2 3 4 5 6)
    ))



(check (and (eq (f 1) '(2 3 4 5 6))
            (eq (f 1 2) '(2 3 4 5 6))))
