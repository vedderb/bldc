
(define r-list '(3 7 12 19 25 8 14 30 5 22 17 11))

(defun cmp (x y)
  {
  (gc)
  (> x y)
  })

(defun f (x)
  (if (= x 0)
      t
    {
    (sort cmp r-list)
    (f (- x 1))
    }))

(check (f 5))
