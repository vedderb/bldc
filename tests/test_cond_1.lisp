

(defun f (x)
  (let ( (a (+ x 5)))
    (cond ( (= a 0) 'zero)
          ( (> a 0) 'positive)
          ( (< a 0) 'negative))))

(check (and (eq (f -5)   'zero)
            (eq (f 0)    'positive)
            (eq (f -100) 'negative)))
