
(def a (range 0 10))
(def b (list-to-array a))
(def c (array-to-list b))

(check (eq a c))
