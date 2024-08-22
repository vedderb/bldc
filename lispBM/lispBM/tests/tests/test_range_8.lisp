
(defun f (n) (if (= n 0)
                 0
               (progn (range 100) (f (- n 1)))))

;; Trigger gc lots of times.
(f 100000)
(check (eq (range 10) '(0 1 2 3 4 5 6 7 8 9)))
