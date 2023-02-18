

(def n (* (word-size) (mem-longest-free)))

(def a (array-create (/ n 4)))

(defun f ()
    (array-create (/ n 4))) ;; probably fine


(spawn-trap f)


(def res (recv ((exit-error (? tid) (? e)) e)
               ((exit-ok    (? tid) (? r)) r)))

(and (eq (type-of res) 'type-array ) ;; OK status cought
     (= (+ 1 2) 3))        ;; eval is alive  
