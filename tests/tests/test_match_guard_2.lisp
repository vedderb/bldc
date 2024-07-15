
(defun f (x)
  (match x
         ( (apa (? x)) (<= x 10) (+ x 1))
         ( (apa (? x)) (>  x 10) (+ x 100))))



(check (and (= (f '(apa 0)) 1)
            (= (f '(apa 2)) 3)
            (= (f '(apa 11)) 111)))
