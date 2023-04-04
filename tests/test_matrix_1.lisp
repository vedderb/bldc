

(def d '(1.0 0.0 0.0
         0.0 1.0 0.0
         0.0 0.0 1.0))

(def m (list-to-matrix 3 d))
                       

(def d-new (matrix-to-list m))

(check (eq d-new d))
