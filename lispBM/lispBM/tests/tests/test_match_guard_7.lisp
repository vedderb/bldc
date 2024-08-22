
(defun f (x)
  (match x
         ( (? x) (< x 5) (list x 'smaller))
         ( (? x) (> x 5) (list x 'larger)) 
         ( _ 'whatever)))

(check (and (eq (f 23) '(23 larger))
            (eq (f 0.3) '(0.3 smaller))))
