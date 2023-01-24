

(def n (* 4 (mem-longest-free)))

(def a (array-create (- n 1500)))

(defun f () (array-create 1500)) ;; Should not succeed


(spawn-trap f)


(def err (recv ((exit-error (? tid) (? e)) e)
               ((exit-ok    (? tid) (? r)) r)))

(and (eq err out_of_memory) ;; error caught
     (= (+ 1 2) 3))         ;; eval is alive
