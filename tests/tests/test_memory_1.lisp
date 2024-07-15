

(defun repeat (n c)
  (if (= n 0) nil
    (progn
      (c)
      (repeat (- n 1) c)
      )))


(def n (* 4 (mem-longest-free)))
(repeat 100 (fn () (bufcreate (- n 100))))

(check t)
