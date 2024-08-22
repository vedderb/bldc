
(defun f (x)
  (let ((a (cond (( < x 0) 'negative)
                 (( = x 0) 'zero)
                 (( > x 0) 'positive))))
    (cond ( (eq a 'positive) 100)
          ( (eq a 'zero)     1000)
          ( (eq a 'negative) 10000))))


(check (and (= (f 0) 1000)
            (= (f 1) 100)
            (= (f -1) 10000)))
