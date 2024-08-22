
(defun f (x)
    (cond ( (> x 0) 'positive)
          ( (< x 0) 'negative)
          ( 't      'zero)))

(check (and (eq (f 0)  'zero)
            (eq (f 1)  'positive)
            (eq (f -1) 'negative)))
