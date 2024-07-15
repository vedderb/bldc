(defun test (a b)
    (cond
        ((= a 0) a)
        (t b)
))

(move-to-flash test)
(check (= (test 10 2) 2))
