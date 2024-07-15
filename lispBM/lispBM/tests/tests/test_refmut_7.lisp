(check (let  ((a (cons 1 2)))
         (progn (setcar a 10)
	        (and (= (car a) 10) (= (cdr a) 2)))))
