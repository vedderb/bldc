(check (= (let ((a 5))
            (progn (let ((a 3)) a) (+ a 10)) 15)))
