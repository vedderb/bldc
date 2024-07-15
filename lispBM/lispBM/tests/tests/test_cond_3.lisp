
(defun f (x)
    (cond ( (> x 0) 'positive)
          ( (< x 0) 'negative)))

(check (and (eq (f 0)  nil)
            (eq (f 1)  'positive)
            (eq (f -1) 'negative)))
