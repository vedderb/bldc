
(defun f (x y)
  (match (cons x y)
         ((1 . 2) 'a-symbol)
         ( _ 'whatever)))

(check (eq (f 1 4) 'whatever))
