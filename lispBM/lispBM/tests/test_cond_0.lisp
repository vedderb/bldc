

(defun b (x) (> x 100))

(defun f (x)
  (cond ( (b x) 'lots)
        ( (> x 50)  'ok)
        ( (> x 0)   'meh)))


(check (and (eq (f 101) 'lots)
            (eq (f 100) 'ok)
            (eq (f 10)  'meh)))
       
