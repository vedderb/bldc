

(defun repeat (n c)
  (if (= n 0) 'repeat-done
    (progn
      (c)
      (repeat (- n 1) c)
      )))



(defun f () {
       (def n (* 4 (mem-longest-free)))
       (repeat 100 (fn () (bufcreate (- n 2000))))
       })



(spawn-trap f)


(check (eq (recv ((exit-error (? tid) (? e)) 'error)
                 ((exit-ok    (? tid) (? r)) r))
           'repeat-done))
