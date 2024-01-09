
(define r-list '(3 7 12 19 25 8 14 30 5 22 17 11))


(defun f (x)
  (if (= x 0)
      t
    {
    (sort > r-list)
    (f (- x 1))
    }))

(check (f 10000))
