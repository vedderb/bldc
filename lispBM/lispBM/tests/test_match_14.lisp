
(defun f (x y)
  (match `(,x . ,y)
         ((1 . 2) 'a-symbol)))

(check (eq (f 1 2) 'a-symbol))
