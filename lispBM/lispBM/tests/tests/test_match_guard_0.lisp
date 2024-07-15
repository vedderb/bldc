
(defun f (x)
  (match x
         ( _ (<= x 10) (+ x 1))
         ( _ (>  x 10) (+ x 100))))



(check (and (= (f 0) 1)
            (= (f 2) 3)
            (= (f 11) 111)))
