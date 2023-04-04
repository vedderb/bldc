(check (let  ((a (cons 1 2)))
         (progn (setcdr a 10)
	        (and (= (car a) 1) (= (cdr a) 10)))))
