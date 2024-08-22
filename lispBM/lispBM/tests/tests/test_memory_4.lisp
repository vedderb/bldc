

(def n (* (word-size) (mem-longest-free)))

(def a (bufcreate (/ n 4)))

(defun f ()
    (bufcreate (/ n 4))) ;; probably fine


(spawn-trap f)


(def res (recv ((exit-error (? tid) (? e)) e)
               ((exit-ok    (? tid) (? r)) r)))

(check (and (eq (type-of res) 'type-array ) ;; OK status cought
            (= (+ 1 2) 3)))                 ;; eval is alive  
