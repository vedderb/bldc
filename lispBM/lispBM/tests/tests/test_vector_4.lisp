
(def v (list-to-vector '(1.0 2.0 3.0 4.0)))

(check (and (> (mag v) 5)
            (< (mag v) 6))) 
