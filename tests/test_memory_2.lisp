

(defun repeat (n c)
  (if (= n 0) 'repeat-done
    (progn
      (c)
      (repeat (- n 1) c)
      )))


(def n (* 4 (mem-longest-free)))
(defun f () (repeat 100 (fn () (array-create (- n 1500)))))



(spawn-trap f)


(eq (recv ((exit-error (? tid) (? e)) 'error)
         ((exit-ok    (? tid) (? r)) r))
   'repeat-done)
