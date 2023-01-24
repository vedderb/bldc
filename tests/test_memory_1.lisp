

(defun repeat (n c)
  (if (= n 0) nil
    (progn
      (c)
      (repeat (- n 1) c)
      )))


(def n (* 4 (mem-longest-free)))
(repeat 100 (fn () (array-create (- n 1))))

t
