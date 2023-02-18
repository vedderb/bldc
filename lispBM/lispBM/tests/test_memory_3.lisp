

(def n (* (word-size) (mem-longest-free)))

(def a (array-create (/ n 2)))

(defun f ()
  (progn
    (+ 1 2)
    (array-create (/ n 2)))) ;; Should not succeed


(spawn-trap f)


(def res (recv ((exit-error (? tid) (? e)) e)
               ((exit-ok    (? tid) (? r)) r)))

(and (eq res out_of_memory) ;; error caught
     (= (+ 1 2) 3))        ;; eval is alive
