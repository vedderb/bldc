(defun test (x)
    (cond
        ((= x 1) 1)
        (t 2)
        ))

(check (= 1 (test 1)))
