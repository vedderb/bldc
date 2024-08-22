(defun f (x i)
  (match x
         ( (apa (? x)) (> x i) 'bigger)
         ( (apa (? x)) (< x i) 'smaller)))



(check (and (eq (f '(apa 0) 1) 'smaller)
            (eq (f '(apa 1) 0) 'bigger)
            (eq (f '(apa 1000) 900) 'bigger)
            (eq (f '(apa 900) 1000) 'smaller)))
