

(defun sum (ls)
  (if (eq ls nil) 0
    (let (( (a . as) ls))
      (+ a (sum as)))))


(check (= (sum (list 1 2 3 4 5 6 7 8 9 10)) 55))
    
