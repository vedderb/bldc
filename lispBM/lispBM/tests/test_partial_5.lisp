(let ((apa 2))
  (defun f (x y z w) (+ apa x y z w)))

(= ((f 1) 2 3 4) 12)
