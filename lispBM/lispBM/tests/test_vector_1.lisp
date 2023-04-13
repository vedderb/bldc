

(def vec (list-to-vector '(1 2 3)))
(def lis (vector-to-list vec))


(check (eq lis '(1.0 2.0 3.0)))
