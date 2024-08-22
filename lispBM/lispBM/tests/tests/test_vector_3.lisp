
(def v (list-to-vector '(1.0 2.0 3.0 4.0)))

(check (and (= (vproj v 0) 1.0)
            (= (vproj v 1) 2.0)
            (= (vproj v 2) 3.0)
            (= (vproj v 3) 4.0)))
