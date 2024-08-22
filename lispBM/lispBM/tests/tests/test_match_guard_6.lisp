
(defun f (x)
  (match x
         ( (? x) (< x 5) 'smaller)
         ( (? x) (> x 5) 'larger) 
         ( _ 'whatever)))

(check (and (eq (f 23) 'larger)
            (eq (f 0.3) 'smaller)))
