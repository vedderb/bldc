
(check (and
        (eq (read (to-str [1])) [1])
        (eq (read (to-str [1 2])) [1 2])
        (eq (read (to-str [1 2 3])) [1 2 3])
        (eq (read (to-str [1 2 3 4])) [1 2 3 4])
        (eq (read (to-str [1 2 3 4 5])) [1 2 3 4 5])))
 
