(def a '(
        (test1 . 1)
        (test2 . 2)
        (test3 . 3)
))

(move-to-flash a)

(check (and (eq (assoc a 'test2) 2)
            (eq (cossa a 3) 'test3)))
